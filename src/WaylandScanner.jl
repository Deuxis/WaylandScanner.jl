"""
The main WaylandScanner module provides a way to scan any Wayland protocol files and generate Julia code for using the protocols.
This can either be done by generating expressions into the module scope at compile time or outputting a Julia script file with the definitions.
"""
module WaylandScanner

export wlparse, genlibclient

import Unicode, Base.show, Base.read, Base.write
using Base: finalizer

include("Parsing.jl")
using .Parsing

# Utils
tojuliaid(identifier) = Symbol(replace(Unicode.titlecase(string(identifier)), "_" => "")) # wl_display to WlDisplay

# Generated library core
"""
	WaylandInterface

The supertype of all Wayland interfaces. Subtypes must either have `id` property which holds their object id or custom [`send_request`](@ref) and [`receive_event`](@ref).

Direct analogy to Wayland's object model. The subtypes represent different interfaces and their instances represent instances of objects implementing these interfaces.
"""
abstract type WaylandInterface end
"""
	WaylandRequestMeta(target, name, opcode, args)

Request meta object. Create an instance only if you want to describe a request.

`target`: the interface the request will be targeted for
`name`: the name of the request
`opcode`: opcode of the request. Depends on the order the request is listed in, counting from 0. For example, wl_display::sync is listed first (and therefore is in the wl_display's requests array at index 1), so its opcode is 0 (index - 1).
`args`: A Vector of name=>type pairs, describing the arguments in order.

Objects of this type are created by the parser from [`SRequest`](@ref)s obtained from the xml file. Opcode is the index of the request in the [`SInterface`](@ref), starting from 0.
"""
struct WaylandRequestMeta
	target::Type{<: WaylandInterface}
	name::Symbol
	opcode::UInt16
	args::Union{Vector{Pair{Symbol, TypeofAbstractWlMsgType}}, Nothing}
end
"""
	WaylandEventMeta

Describes an event. Create an instance only if you want to describe an event - for operating on events use [`WaylandEvent`](@ref)
"""
struct WaylandEventMeta
	source::Type{<: WaylandInterface}
	name::Symbol
	opcode::UInt16
	args::Union{Vector{Pair{Symbol, TypeofAbstractWlMsgType}}, Nothing}
end
"""
	WaylandEvent{object, ename}(opcode, args)

Decoded event message. Parametrised so you can match an event "name" from wayland object "obj" with WaylandEvent{obj, :name}
"""
struct WaylandEvent{object, ename}
	source::WaylandInterface
	name::Symbol
	opcode::UInt16
	args::Vector{WlMsgType}
	WaylandEvent{object, ename}(opcode, args) where ename where object = new(object, ename, opcode, args)
end

struct WaylandEnum
	owner # WaylandInterfaceMeta, but circular dependency
	name::Symbol
	entries::AbstractDict{Symbol, WlUInt}
end

const RequestDict = Dict{Symbol, WaylandRequestMeta}
const EventDict = Dict{Symbol, WaylandEventMeta}
const EnumDict = Dict{Symbol, WaylandEnum}

struct WaylandInterfaceMeta
	requests::RequestDict
	events::EventDict
	enums::EnumDict
end
const InterfaceDict = Dict{Symbol, WaylandInterfaceMeta}

let current_newid = 0 # Because we return the value of `+= 1`, the first ID returned is 1.
	"""
		get_newid()

	Gets the next free ID in client range, which is [1, 0xff000000)
	"""
	global get_newid() = current_newid < 0xff000000 ? current_newid += 1 : current_newid = 1 # TODO: Check if it's not still used, even after a full loop. (Not necessary for testing, as a testing client will never go through 16 777 215 IDs)
end
#="""
	used_ids

A Set of IDs that are in use, updated every time an object gets created by us, and when it gets destroyed. NYI
"""
used_ids = Set{WlID}=#
"""
	send_request(io::IO, from::WlID, opcode, args::WlMsgType...)

Create the request message and send it to the IO.

This is part of the low level interface used by the generated library. You probably want the generated functions instead.
"""
function send_request(io::IO, from::WlID, opcode, args::WlMsgType...)
	size = 8 # just the header
	if isempty(args)
		payload = nothing
	else
		payload = IOBuffer()
		for arg in args
			size += write(payload, arg)
		end
	end
	send(io, GenericMessage(from, size, opcode, payload))
end
"""
	send_request(to::WaylandInterface, from::WlID, opcode, args::WlMsgType...)

[`send_request`](@ref) to the given object's ID.
"""
send_request(to::WaylandInterface, from::WlID, opcode, args::WlMsgType...) = send_request(to.id, from, opcode, args...)
"""
    receive_event(io::IO; msg_type=GenericMessage)

Receive an event message and decode it into a `WaylandEvent` object.

This is part of the low level interface used by the generated library. You probably want the generated high-level event listening utilities instead.
"""
function receive_event(io::IO, msg_type=GenericMessage)
	receive(io, msg_type)
end

# Library generation
"""
	genrequest(meta)

Generate a request function from meta.

meta must contain fields:
.target: a `WaylandInterface` subtype - type of the object the request will be sent to
.name: a `Symbol` which will become the function name
.opcode: a `UInt16` - the request opcode
.args: an ordered iterable collection of `name=>Type` `Pair{Symbol,TypeofWlMsgType}`s describing the (`name::Type`) arguments.

new_id arguments aren't presented to the user, but are acquired by the get_newid call in send_request arguments.
"""
function genrequest(meta)
	if meta.args == nothing
		:( $(meta.name)(target::$(meta.target)) = send_request(target, $(meta.opcode)) )
	else
		head_args = Vector{Expr}()
		tail_args = Vector{Expr}()
		for (name, type) in meta.args
			if type == WlNewID
				push!(tail_args, :(get_newid()))
			else
				push!(head_args, :($name::$type))
				push!(tail_args, name)
			end
		end
		:( $(meta.name)(target::$(meta.target), $(head_args...)) = send_request(target, $(meta.opcode), $(tail_args...)) )
	end
end
"""
	ListenerRegistry

As the name says. Dictionary mapping event types to `Set`s of their listeners.
"""
ListenerRegistry = Dict{WaylandEvent, Set{Function}}
"""
	GlobalListenerRegistry

Global, default instance of [`ListenerRegistry`](@ref).
"""
global_listener_registry = ListenerRegistry()
"""
	addlistener(f, event::Type{<: WaylandEvent}, data = nothing)

Register `f` to be called every time an event of `event` type gets fired.

The default implementations only pass one argument to `f` – the event object itself – if `data` is set to nothing, and two arguments – the event object as first and `data` as second – otherwise.
"""
addlistener(f, event::Type{WaylandEvent}) = error("Generic `addlistener` called for $(event)!")
"""
	remlistener(f, event::Type{WaylandEvent}, throws = true)

Remove `f` from registered listeners on event type `event`, returning `f`. If `throws` is true, throws ArgumentError if `f` is not registered as listener on `event`, otherwise returns false in that case.
"""
remlistener(f, event::Type{WaylandEvent}, throws = true) = error("Generic `remlistener` called for $(event)!")
"""
    genlibclient(protocols::Set{SProtocol})

Generate the client library. Entry point of the generation step.

The generated library consists of a internally used interface dictionary of type InterfaceDict holding metadata, and structs and methods meant for direct use.

The interface dictionary is a Dict which maps a Symbol interface name to a simple WaylandInterfaceMeta object, consisting only of three dictionaries: `requests::RequestDict`, `events::EventDict` and `enums::EnumDict`. It's the easiest way to check enum values and events to register listeners to.

The generated structs are subtypes of [`WaylandInterface`](@ref) and are described there, while methods are requests – which have the names of the requests they represent – and `addlistener` + `remlistener` duo for listening to events. For example, parsing `wl_display` interface will generate a `WlDisplay` struct for representing such object, a [`get_registry(target::WlDisplay)`](@ref) method for sending its `get_registry` request and [`addlistener(f, event::Type{WaylandEvent{ID, :error}}) where ID`](@ref), [`remlistener(f, event::Type{WaylandEvent{ID, :error}}) where ID`](@ref) duo for interacting with its "error" events.

By default there is only one InterfaceDict holding all known interfaces, returned from one call to genlibclient. However, if the separation of loaded protocols is desired, it is entirely possible to call this function multiple times, resulting in multiple working dictionaries and multiple but indistinguishable sets of generated methods. (Do note that in the event of repeating interface name there will be a namespace clash of generated globally-visible structs and, if they have the same named requests/events, the methods)

To be implemented:

- Treating destructor requests differently so that they get processed automatically without the need for the user to manually call them.
"""
function genlibclient(protocols::Set{SProtocol})
	ifs = Dict{Symbol, WaylandInterfaceMeta}()
	for prot in protocols
		for interface in prot.interfaces
			# Generate the interface type.
			@eval begin
				interface = $interface
				iftypename = tojuliaid(interface.name)
				"""
					$iftypename

				Represents a $(interface.name) object.
				"""
				struct $iftypename <: WaylandInterface # Visible in global scope since that's where [`eval`](@ref) executes.
					id::WlID
				end
				iftype = $iftypename
			end
			reqs = RequestDict()
			for (index, request) in enumerate(interface.requests)
				if request.args == nothing
					args = nothing
				else
					args = Vector{Pair{Symbol, TypeofAbstractWlMsgType}}()
					for arg in request.args
						push!(args, Symbol(arg.name) => arg.type)
					end
				end
				request_meta = WaylandRequestMeta(iftype, Symbol(request.name), index - 1, args)
				push!(reqs, request_meta.name => request_meta)
				# [`eval`](@ref) executes in module's global scope, so the request methods along with their docs will too be visible.
				docargstring = "target::$(request_meta.target)"
				if args != nothing
					for (name, type) in args
						docargstring *= ", $name::$type"
					end
				end
				"""
					$(request_meta.name)($docargstring)

				$(repr(request.description))
				"""
				eval(genrequest(request_meta))
			end
			evs = EventDict()
			for (index, event) in enumerate(interface.events)
				if event.args == nothing
					args = nothing
				else
					args = Vector{Pair{Symbol, TypeofAbstractWlMsgType}}()
					for arg in event.args
						push!(args, Symbol(arg.name) => arg.type)
					end
				end
				event_meta = WaylandEventMeta(iftype, Symbol(event.name), index - 1, args)
				push!(evs, event_meta.name => event_meta)
				# Add the event to global listener registry
				event_type = WaylandEvent{iftype, event_meta.name}
				push!(global_listener_registry, event_type => Set{Function}())
				# not sure if eval is needed here
				@eval begin
					function addlistener(f::Function, ::Type{WaylandEvent{$iftype, $(event_meta.name)}})
						push(global_listener_registry[$event_type], f)
					end
					function remlistener(f::Function, ::Type{WaylandEvent{$iftype, $(event_meta.name)}}, throws = true)
						if f in global_listener_registry[$event_type]
							delete!(global_listener_registry, f)
							return f
						else
							if throws
								throw(ArgumentError("$f wasn't registered in global_listener_registry."))
							else
								return false
							end
						end
					end
				end
			end
			ens = EnumDict()
			ifmeta = WaylandInterfaceMeta(reqs, evs, ens) # needed here and not in the push!() below because it's needed for WaylandEnum construction
			for enum in interface.enums
				entries = Dict{Symbol, WlUInt}()
				for (name, entry) in enum.entries
					push!(entries, name => entry.value)
				end
				push!(ens, Symbol(enum.name) => WaylandEnum(ifmeta, Symbol(enum.name), entries))
			end
			push!(ifs, Symbol(interface.name) => ifmeta)
		end
	end
	return ifs
end
"""
	genlibclient(path::AbstractString)

Generate the client library from a protocol XML file denoted by `path`.

Equivalent to calling `genlibclient(wlparse(path))`.
"""
genlibclient(path::AbstractString) = genlibclient(wlparse(path))
"""
	genlibclient(paths::Set{AbstractString})

Generate the client library from a Set of protocol XML files.

Equivalent to calling `genlibclient(reduce(append!, wlparse.(paths)))`.
"""
genlibclient(paths::Set{AbstractString}) = genlibclient(reduce(append!, wlparse.(paths)))

end  # module WaylandScanner
