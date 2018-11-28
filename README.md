# WaylandScanner.jl
A Julia module for parsing Wayland protocol files and :unicorn:generating Julia bindings. In the future this module will hopefully be able to generate full Wayland bindings that can be statically compiled into something that doesn't even require a Julia runtime.

Heavily work in progress.

| Functionality | Status |
| ------------- | ------ |
| parsing XML protocol files | mostly done
| pretty-printing the parsed tree | mostly done
| generating Julia bindings | not even started

## Usage
Download the file and put it somewhere, where Julia will find it. Then `import WaylandScanner`.
To parse a protocol file use `wlparse("path/to/file")`. This returns a `Set` of `SProtocol` objects, which represent the abstract tree of the protocol. The entire tree is documented in code and (except for enums and formatted strings, which are still ugly) can be pretty-printed by the Julia REPL.

## Dependencies
The only direct dependencies are a working 1.0-compatible Julia runtime and the imported modules, LightXML and FixedPointNumbers, but LightXML depends on libxml2.
