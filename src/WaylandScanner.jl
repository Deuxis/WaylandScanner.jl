module WaylandScanner

export wlparse, genlibclient

import Base.show, Base.read, Base.write
using Base: finalizer
using WaylandCore
using LightXML

# Parsing
@enum RequestType none destructor
"""
	Scanner types

These corespond to XML descriptions, with comments stating how they are supplied in the XML. Some "required" by the format fields aren't actually necessary to function, so we'll asume they might be missing and just generate a warning if that's the case.
Ordering of member values:
primary:
1. attribute
2. single child node
3. multiple child nodes
secondary:
1. required
2. optional
3. implied
"""
abstract type ScannerStruct end
struct SDescription <: ScannerStruct
	summary::Union{AbstractString,Nothing} # attribute, required
	content::Union{AbstractString,Nothing} # childnode (text content of the element), required
end
struct SEnumEntry <: ScannerStruct
	name::AbstractString # attribute, required. May be redundant because the entries are accessed in the Dict by name anyway.
	value::WlUInt # attribute, required
	since::WlVersion # attribute, implied
	summary::Union{AbstractString,Nothing} # attribute, implied
	description::Union{SDescription,Nothing} # childnode, optional
end
struct SEnum <: ScannerStruct
	name::AbstractString # attribute, required
	since::WlVersion # attribute, implied
	bitfield::Bool # attribute, implied. Signifies if the enum is a plain, C-like enum, or a multi-choice bitfield
	description::Union{SDescription,Nothing} # childnode, optional
	entries::Dict{Symbol,SEnumEntry} # childnodes, required
end
struct SArgument <: ScannerStruct
	name::AbstractString # attribute, required. If the XML is non-compliant, this may be non-existant and still able to work, but the arguments to actual functions are named after it, so making it optional is TODO.
	type::TypeofAbstractWlMsgType # attribute, required
	summary::Union{AbstractString,Nothing} # attribute, implied
	interface::Union{AbstractString,Nothing} # attribute, implied. If the argument is of type "object", this describes its interface. XXX: Needs later conversion!
	nullable::Bool # attribute, implied
	enum::Union{AbstractString,Nothing} # attribute, implied. If the argument is actually an enum (has int or uint type and this attribute set), specifies which one is it. XXX: Needs later conversion!
	description::Union{SDescription,Nothing} # childnode, optional
end
struct SEvent <: ScannerStruct
	name::AbstractString # attribute, required
	since::WlVersion # attribute, implied
	description::Union{SDescription,Nothing} # childnode, optional
	args::Union{Vector{SArgument}, Nothing} # childnodes, optional
end
struct SRequest <: ScannerStruct
	name::AbstractString # attribute, required
	since::WlVersion # attribute, implied
	rtype::RequestType # attribute, implied
	description::Union{SDescription,Nothing} # childnode, optional
	args::Union{Vector{SArgument}, Nothing} # childnodes, optional
end
struct SInterface <: ScannerStruct
	name::AbstractString # attribute, required
	version::WlVersion # attribute, required
	description::Union{SDescription,Nothing} # childnode, optional
	requests::Vector{SRequest} # childnodes, optional
	events::Vector{SEvent} # childnodes, optional
	enums::Set{SEnum} # childnodes, optional
end
struct SProtocol <: ScannerStruct
	name::AbstractString # attribute, required
	description::Union{SDescription,Nothing} # childnode, optional
	interfaces::Set{SInterface} # childnodes, required (at least one)
end
# scanner structs helpers
"""
    imply_since(s)

Imply the "since" attribute, resolving to 1 if it's not present and valid.
"""
imply_since(::Nothing) = 1
imply_since(s::AbstractString) = (since = tryparse(WlVersion, s)) == nothing ? nothing : since
function parse_wlmsgtype(ts::AbstractString)::TypeofAbstractWlMsgType
	if ts == "int"
		WlInt
	elseif ts == "uint"
		return WlUInt
	elseif ts == "fixed"
		WlFixed
	elseif ts == "string"
		WlString
	elseif ts == "object"
		WlObjID
	elseif ts == "new_id"
		WlNewID
	elseif ts == "array"
		WlArray
	elseif ts == "fd"
		WlFD
	else
		throw(ArgumentError("Wayland message argument type \"$ts\" not recognised."))
	end
end
"""
    flatten(s::AbstractString)

Flatten a formatted string into a simple, one-line one. Strip all trailing and leading whitespace and change all interior whitespace into a single space.
"""
flatten(s::AbstractString) = replace(strip(s), r"\s+"=>" ")
"""
	ScannerStruct(::XMLElement)

Do note that all these constructors trust the element to actually be their XML counterpart, the caller is responsible for supplying correct element nodes.
"""
function SProtocol(element::XMLElement)
	name = attribute(element, "name"; required=true)
	_description = find_element(element, "description")
	description = _description == nothing ? nothing : SDescription(_description)
	interfaces	= Set{SInterface}()
	for child in child_elements(element)
		if LightXML.name(child) == "interface"
			push!(interfaces, SInterface(child))
		end
	end
	SProtocol(name, description, interfaces)
end
function SInterface(element::XMLElement)
	name = attribute(element, "name"; required=true)
	_version = attribute(element, "version")
	version  = _version == nothing ? 1 : parse(WlVersion, _version)
	_description = find_element(element, "description")
	description  = _description == nothing ? nothing : SDescription(_description)
	enums = Set{SEnum}()
	requests = Vector{SRequest}()
	events = Vector{SEvent}()
	for child in child_elements(element)
		childname = LightXML.name(child)
		if childname == "enum"
			push!(enums, SEnum(child))
		elseif childname == "request"
			push!(requests, SRequest(child))
		elseif childname == "event"
			push!(events, SEvent(child))
		end
	end
	SInterface(name, version, description, requests, events, enums)
end
function SRequest(element::XMLElement)
	name = attribute(element, "name"; required=true)
	since = imply_since(attribute(element, "since"))
	_rtype = attribute(element, "type")
	if	_rtype == "destructor"
		rtype = destructor
	else
		rtype = none
	end
	_description = find_element(element, "description")
	description = _description == nothing ? nothing : SDescription(_description)
	args = Vector{SArgument}()
	for child in child_elements(element)
		if LightXML.name(child) == "arg"
			push!(args, SArgument(child))
		end
	end
	isempty(args) && (args = nothing)
	SRequest(name, since, rtype, description, args)
end
function SEvent(element::XMLElement)
	name = attribute(element, "name"; required=true)
	since = imply_since(attribute(element, "since"))
	_description = find_element(element, "description")
	description = _description == nothing ? nothing : SDescription(_description)
	args = Vector{SArgument}()
	for child in child_elements(element)
		if LightXML.name(child) == "arg"
			push!(args, SArgument(child))
		end
	end
	isempty(args) && (args = nothing)
	SEvent(name, since, description, args)
end
function SEnum(element::XMLElement)
	name = attribute(element, "name"; required=true)
	since = imply_since(attribute(element, "since"))
	_bitfield = attribute(element, "bitfield")
	if	_bitfield == nothing
		bitfield = false
	else
		bitfield = parse(Bool, _bitfield)
	end
	_description = find_element(element, "description")
	description = _description == nothing ? nothing : SDescription(_description)
	entries = Dict{Symbol,SEnumEntry}()
	for child in child_elements(element)
		if LightXML.name(child) == "entry"
			entry = SEnumEntry(child)
			entries[Symbol(entry.name)] = entry
		end
	end
	SEnum(name, since, bitfield, description, entries)
end
function SEnumEntry(element::XMLElement)
	name = attribute(element, "name"; required=true)
	value = parse(WlUInt, attribute(element, "value"; required=true))
	since = imply_since(attribute(element, "since"))
	_summary = attribute(element, "summary")
	_description = find_element(element, "description")
	if	_description == nothing
		description = nothing
		summary = _summary
	else
		description = SDescription(_description)
		if _summary == nothing
			summary = description.summary
		else
			summary = _summary
		end
	end
	SEnumEntry(name, value, since, summary, description)
end
function SArgument(element::XMLElement)
	name = attribute(element, "name"; required=true)
	type = parse_wlmsgtype(attribute(element, "type"; required=true))
	if type != WlID
		interface = nothing
	else
		interface = attribute(element, "interface")
	end
	_nullable = attribute(element, "nullable")
	if	_nullable == nothing
		nullable = false
	else
		nullable = parse(Bool, _nullable)
	end
	if type <: Integer
		enum = attribute(element, "enum")
	else
		enum = nothing
	end
	_description = find_element(element, "description")
	_summary = attribute(element, "summary")
	if	_description == nothing
		description = nothing
		summary = _summary
	else
		description = SDescription(_description)
		if _summary == nothing
			summary = description.summary
		else
			summary = _summary
		end
	end
	SArgument(name, type, summary, interface, nullable, enum, description)
end
function SDescription(element::XMLElement)
	econtent = flatten(content(element))
	econtent = econtent == "" ? nothing : econtent
	SDescription(attribute(element, "summary"), econtent)
end

# utility functions
stype(::SProtocol) = "Protocol"
stype(::SInterface) = "Interface"
stype(::SRequest) = "Request"
stype(::SEvent) = "Event"
stype(::SEnum) = "Enum"
stype(::SEnumEntry) = "Enum entry"
stype(::SArgument) = "Argument"
stype(::SDescription) = "Description"
"""
    SCollection

Any collection of `ScannerStruct`s.
"""
const SCollection = Union{Set{<: ScannerStruct},Vector{<: ScannerStruct},Dict{<: Any,<: ScannerStruct}}
"""
    getindent(count::Integer, s::AbstractString = "\t")

Get an indent string from the indentation depth (count) and a string representing an indentation level.
"""
getindent(count::Integer, s::AbstractString = "\t") = repeat(s, count)
"""
	indent1

The string representing a single indent level.
"""
indent1 = getindent(1)
"""
	show(io::IO, mime::MIME"text/plain". o::ScannerStruct)

Show a ScannerStruct in a human readable indented format.

Overriden with more specific methods when needed.
"""
function show(io::IO, mime::MIME"text/plain", o::ScannerStruct)
	indent_level = get(io, :indent, 0)
	indent = getindent(indent_level, indent1)
	print(io, "$indent$(stype(o)) $(o.name):\n")
	for fname in fieldnames(typeof(o))
		prop = getproperty(o, fname)
		if prop == nothing
			print("$indent$indent1$fname: none")
		elseif prop isa ScannerStruct
			print("$indent$indent1$fname:\n")
			show(IOContext(io, :indent=>indent_level + 2), mime, prop)
		elseif prop isa SCollection
			print(io, "$indent$indent1$fname:")
			show(IOContext(io, :indent=>indent_level + 2), mime, prop)
		else
			print(io, "$indent$indent1$fname: $(repr(MIME("text/plain"), prop))")
		end
		print(io, '\n')
	end
end
"""
    show(io::IO, mime::MIME"text/plain", o::Set{<: ScannerStruct})

Pretty-print a collection of [`ScannerStruct`](@ref)s.
"""
function show(io::IO, mime::MIME"text/plain", o::SCollection)
	if isempty(o)
		print(io, " none")
	else
		print(io, '\n')
		indent_level = get(io, :indent, 0)
		for member in o
			show(IOContext(io, :indent=>indent_level), mime, member)
		end
	end
end
"""
    show(io::IO, mime::MIME"text/plain", o::Dict{Symbol,<: ScannerStruct})

Pretty-print a [`Symbol`](@ref) [`Dict`](@ref) of [`ScannerStruct`](@ref)s.
"""
function show(io::IO, mime::MIME"text/plain", o::Dict{Symbol,<: ScannerStruct})
	indent_level = get(io, :indent, 0)
	indent = getindent(indent_level, indent1)
	print(io, '\n')
	for (key, val) in o
		print(io, "$indent$key:\n")
		show(IOContext(io, :indent=>indent_level + 1), mime, val)
	end
end
"""
	show(io::IO, ::MIME"text/plain", o::SDescription)

Show an [`SDescription`](@ref) in a human readable indented format, shortening the strings.
"""
function show(io::IO, ::MIME"text/plain", o::SDescription)
	if o.summary == nothing && o.content == nothing
		show(io, o)
	else
		indent_level = get(io, :indent, 0)
		indent = getindent(indent_level, indent1)
		summary = o.summary == nothing ? "none" : length(o.summary) >= 100 ? o.summary[1:100] * "…" : o.summary
		content = o.content == nothing ? "none" : length(o.content) >= 100 ? o.content[1:100] * "…" : o.content
		print(io, "$(indent)Description:\n$(indent)$(indent1)summary: $summary\n$(indent)$(indent1)content: $content")
	end
end

"""
    wlparse(element::XMLElement)

Parse an [`XMLElement`](@ref) into a `Set{SProtocol}`.

Returns a `Set{SProtocol}` so that when parsing multiple protocol files or even a file with multiple protocols they can all be easily merged via `append!`.

Example:
```julia
	import LightXML
	protocols = Set{SProtocol}()
	for file in filenames
		xdoc = LightXML.parse_file(file)
		finalizer(free, xdoc)
		append!(protocols, wlparse(LightXML.root(xdoc)))
	end
```
"""
function wlparse(element::XMLElement)
	protocols = Set{SProtocol}()
	if name(element) == "protocol"
		push!(protocols, SProtocol(element))
	else
		for child in child_elements(element)
			append!(wlparse(child))
		end
	end
	return protocols
end
"""
    wlparse(path::AbstractString)

Parse a file denoted by `path`.

Examples:
```julia
# Parse the base protocol file
wayland_protocol = wlparse("/usr/share/wayland/wayland.xml")
# Parse a collection of files
protocol_files = Set("/usr/share/wayland/wayland.xml", "/usr/share/wayland-protocols/stable/xdg-shell")
protocols = reduce(append!, wlparse.(protocol_files))
```
"""
function wlparse(path::AbstractString)
	xdoc = parse_file(path)
	finalizer(free, xdoc)
	wlparse(root(xdoc))
end

"""
    wlparse()

Parse the default Wayland protocol located at /usr/share/wayland/wayland.xml
"""
function wlparse()
	wlparse("/usr/share/wayland/wayland.xml")
end

# Generated library core
"""
There are two actual message types, request and event. Request is a message we send, event is one we receive and need to listen to. The entire rest is up to us.

When it comes to structure, I've chosen a simple approach. Don't care about protocols, as interfaces need to be unique anyway. Requests and `addlistener(listener, event)` methods are global and differentiated by multiple dispatch. Each interface acts as a collection of its requests, events and enums.
Interfaces are subtypes of WaylandInterface.
Because requests, events and enums may share names (for example `wl_display` has an `error` event and an `error` enum), their specific collections are separated into `requests`, `events` and `enums` properties of the `WaylandInterface`, which is an instance of an interface. However, for convenience the `WaylandInterface` itself acts as a collection of (request, event, enum) tuples.

Examples:
```julia-repl
# Accessing a tuple (any non-existing value gets replaced by default_value _in the tuple_):
julia> get(wl_display, :error, nothing)
(nothing, WaylandEventMeta{wl_display, :error}, WaylandEnum(wl_display, :error, :invalid_object=>0, :invalid_method=>1, :no_memory=>2))
# Accessing an event:
julia> error_event = get(wl_display.events, :error, nothing)
WaylandEventMeta{wl_display, :error}
# Launching a request:
julia> get_registry(wl_display)
# Launching a request with arguments:
julia> bind(wl_registry, 1)
# Adding an event listener:
julia> addlistener(listener, error_event)
```
"""
"""
	WaylandInterface

The supertype of all Wayland interfaces. Subtypes must either have `id` property which holds their object id or custom [`send_request`](@ref) and [`receive_event`](@ref).

Direct analogy to Wayland. The subtypes represent different interfaces and their instances represent instances of objects implementing these interfaces.
"""
abstract type WaylandInterface end
"""
	WaylandRequestMeta{object, rname}(opcode, args)

Request meta object. Describes a request and allows dispatching on concrete requests. Create an instance only if you want to describe a request - for dispatching use the automatically generated instance or the parametrised type itself.

`target` (and the `object` parameter): the interface the request will be targeted for
`name`: the name of the request
`opcode`: opcode of the request. Depends on the order the request is listed in, counting from 0. For example, wl_display::sync is listed first (and therefore is in the wl_display's requests array at index 1), so its opcode is 0 (index - 1).
`args`: A Vector of name=>type pairs, describing the arguments in order.

As an example, for the `get_registry` request of `wl_display` the Scanner will create this:
```julia
# opcode is determined from request index during iteration
rname = :get_registry
wl_display.requests[rname] = WaylandRequestMeta{wl_display.interface, rname}(opcode, [:registry=>WlNewID])
```
Such struct semantically describes the `wl_display::get_registry` request of the Wayland Protocol and corresponds to the generated C API function `wl_display_get_registry(wl_display * display, new_id_t registry)`
Then, from that, the Scanner will generate the request function:
```julia
# get_registry(display::WlDisplay)
```
new_id arguments aren't presented to the user, but are acquired by the get_newid call inside the function, so the result in this case is a request function with just one argument, the target.
"""
struct WaylandRequestMeta
	target::Type{<: WaylandInterface}
	name::Symbol
	opcode::UInt16
	args::Union{Vector{Pair{Symbol, TypeofAbstractWlMsgType}}, Nothing}
end
"""
	struct WaylandEventMeta

Describes an event. Create an instance only if you want to describe an event - for dispatching use the automatically generated instance or the parametrised type itself.
"""
struct WaylandEventMeta
	source::Type{<: WaylandInterface}
	name::Symbol
	opcode::UInt16
	args::Union{Vector{Pair{Symbol, TypeofAbstractWlMsgType}}, Nothing}
end
"""
	struct WaylandEvent{object, ename}(opcode, args)

Decoded event message.
"""
struct WaylandEvent{object, ename}
	source::WlID
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

"""
	current_newid

A simple counter state for [`get_newid`](@ref)
"""
let current_newid = 0 # Because we return the value of `+= 1`, the first ID returned is 1.
	"""
		get_newid()

	Gets the next free ID in client range, which is [1, 0xff000000)
	"""
	global get_newid() = current_newid < 0xff000000 ? current_newid += 1 : current_newid = 1
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
"""
function genrequest(meta)
	if meta.args == nothing
		:( $(meta.name)(target::$(meta.target)) = send_request(target, $(meta.opcode)) )
	else
		head_args = Vector{Expr}()
		tail_args = Vector{Expr}()
		for (name, type) in meta.args
			if type == WlNewID # new_id means we only have to supply an automatically generated ID to the send_request call
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
	genrequest(meta)

Macro version of [`genrequest`](@ref)
"""
macro genrequest(meta)
	genrequest(meta)
end
"""
    genlibclient(protocols::Set{SProtocol})

Generate the client library. Entry point of the generation step.

The generated library consists of a internally used interface dictionary of type InterfaceDict holding metadata, and structs and methods meant for direct use.

The interface dictionary is a Dict which maps a Symbol interface name to a simple WaylandInterfaceMeta object, consisting only of three dictionaries: `requests::RequestDict`, `events::EventDict` and `enums::EnumDict`. It's the easiest way to check enum values and events to register listeners to.

The generated structs are subtypes of [`WaylandInterface`](@ref) and are described there, while methods are requests – which have the names of the requests they represent – and `addlistener` + `remlistener` duo for listening to events. For example, parsing `wl_display` interface will generate a `WlDisplay` struct for representing such object, a [`get_registry(target::WlDisplay)`](@ref) method for sending its `get_registry` request and [`addlistener(f, event::Type{WaylandEvent{ID, :error}}) where ID`](@ref), [`remlistener(f, event::Type{WaylandEvent{ID, :error}}) where ID`](@ref) duo for interacting with its "error" events.

By default there is only one InterfaceDict holding all known interfaces, returned from one call to genlibclient. However, if the separation of loaded protocols is desired, it is entirely possible to call this function multiple times, resulting in multiple working dictionaries and multiple but indistinguishable sets of generated methods.
"""
function genlibclient(protocols::Set{SProtocol})
	ifs = Dict{Symbol, WaylandInterfaceMeta}()
	for prot in protocols
		for interface in prot.interfaces
			# Generate the interface type.
			iftypename = Unicode.titlecase(replace(interface.name, r"_" => '')) # wl_display to WlDisplay
			@eval begin
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
				push!(evs, Symbol(event.name) => WaylandEventMeta(iftype, Symbol(event.name), index - 1, args))
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
	genlibclient(path::String)

Generate the client library from a protocol XML file denoted by `path`.

Equivalent to calling `genlibclient(wlparse(path))`.
"""
genlibclient(path::String) = genlibclient(wlparse(path))
"""
	genlibclient(paths::Set{String})

Generate the client library from a Set of protocol XML files.

Equivalent to calling `genlibclient(reduce(append!, wlparse.(paths)))`.
"""
genlibclient(paths::Set{String}) = genlibclient(reduce(append!, wlparse.(paths)))

end  # module WaylandScanner
