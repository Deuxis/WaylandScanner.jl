"""
This part of the scanner constructs the initial dictionary with the contents of the XML.
"""
module Parsing

using LightXML
using ..WaylandCore

# Explicit Base imports for adding methods
import Base.string, Base.show

export wlparse, SProtocol, SInterface, SRequest, SEvent, SArgument, SEnum, SEnumEntry, SDescription

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
	name::AbstractString # attribute, required. If the XML is non-compliant, this may be non-existent and still able to work, but the arguments to actual functions are named after it, so making it optional is TODO.
	type::TypeofWlMsgType # attribute, required
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
	enums::Vector{SEnum} # childnodes, optional
end
struct SProtocol <: ScannerStruct
	name::AbstractString # attribute, required
	description::Union{SDescription,Nothing} # childnode, optional
	interfaces::Vector{SInterface} # childnodes, required (at least one)
end
# scanner structs helpers
"""
    imply_since(s)

Imply the "since" attribute, resolving to 1 if it's not present and valid.
"""
imply_since(::Nothing) = 1
imply_since(s::AbstractString) = (since = tryparse(WlVersion, s)) == nothing ? nothing : since
function parse_wlmsgtype(ts::AbstractString)::TypeofWlMsgType
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
    string(desc::SDescription)

Converts an SDescription into a plain string.
"""
function string(desc::SDescription)::String
	if desc.summary != nothing
		if desc.content != nothing
			return "$(desc.summary)\n\n$(desc.content)"
		else
			return desc.summary
		end
	else
		if desc.content != nothing
			return desc.content
		else
			return ""
		end
	end
end
"""
	ScannerStruct(::XMLElement)

Do note that all these constructors trust the element to actually be their XML counterpart, the caller is responsible for supplying correct element nodes.
"""
function SProtocol(element::XMLElement)
	name = attribute(element, "name"; required=true)
	_description = find_element(element, "description")
	description = _description == nothing ? nothing : SDescription(_description)
	interfaces	= Vector{SInterface}()
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
	enums = Vector{SEnum}()
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

const DEFAULT_PROTOCOL_PATH = "/usr/share/wayland/wayland.xml"

"""
    wlparse(path::AbstractString)

Parse a file denoted by `path`.

Examples:
```julia
# Parse the base protocol file
wayland_protocol = wlparse("$DEFAULT_PROTOCOL_PATH")
# Parse a collection of files
protocol_files = Set("$DEFAULT_PROTOCOL_PATH", "/usr/share/wayland-protocols/stable/xdg-shell")
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

Parse the default Wayland protocol located at `$DEFAULT_PROTOCOL_PATH`.
"""
function wlparse()
	wlparse(DEFAULT_PROTOCOL_PATH)
end

end #module Parsing
