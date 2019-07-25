"""
The main WaylandScanner module provides a way to scan any Wayland protocol files and generate Julia code for using the protocols.
This can either be done by generating expressions into the module scope at compile time or outputting a Julia script file with the definitions.
"""
module WaylandScanner

export tostructid, wlparse, genprotocol, generate_script, output_script

using WaylandCore

import Unicode, Base.read, Base.write
using Base: finalizer

# Parsing
include("Parsing.jl")
using .Parsing

# Utils
tostructid(identifier) = Symbol(replace(Unicode.titlecase(string(identifier)), "_" => "")) # wl_display to WlDisplay

# Generation
include("Generation.jl")
using .Generation

end  # module WaylandScanner
