# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This OCaml project provides a command-line tool for PCB design calculations with three main functions:

1. **Trace resistance calculation** - Calculate electrical resistance of copper traces with temperature compensation
2. **Spiral coil resistance calculation** - Calculate total resistance of multi-turn spiral coils
3. **KiCad footprint generation** - Generate .kicad_mod files for multi-layer spiral coil footprints

## Build and Development Commands

### Building

```bash
dune build
```

### Running Commands

```bash
# Trace resistance calculation
dune exec -- bin/main.exe trace 0.01 -w 0.5mm -t 2oz --temp 85

# Spiral coil resistance calculation
dune exec -- bin/main.exe coil --pitch 0.6 --turns 2.5 --width 0.2 --round 6

# KiCad footprint generation
dune exec -- bin/main.exe kicad --pitch 0.64 --width 0.5 --clearance 0.14 --turns 5 --layers 3 --oval 13.9x6.22 --via-size 0.3/0.15 --output coil.kicad_mod
```

### Testing

When testing the kicad command, don't specify the --output parameter. The footprint will be printed to the standard output.

## Code Architecture

### Multi-Command Structure

The tool uses cmdliner to provide three subcommands (`trace`, `coil`, `kicad`) from a single executable in `bin/main.ml`. Each subcommand has distinct argument parsing and delegates to library functions.

### Library Organization (`lib/`)

**trace.ml** - Core physics calculations:

- Copper resistivity constants and temperature coefficients
- `calculate_resistance` - Implements R = ρL/A with temperature compensation
- Unit conversion utilities for PCB industry standards (oz/ft², mils)

**coil.ml** - Spiral coil geometry and generation:

- `coil_shape` sum type supporting Round, Square, Rectangular, and Oval geometries
- `generate_spiral_segments` - Creates path segments for multi-layer coils with automatic layer mirroring
- Transformation matrices for coordinate mirroring and rotation
- Layer-aware segment generation with `layer_segments` grouping

**kicad.ml** - KiCad footprint serialization:

- S-expression generation using sexplib0 with custom formatting functions
- Complete KiCad footprint type system including pads, primitives, and metadata
- `generate_footprint` - Converts coil segments to .kicad_mod format
- Automatic via generation for even-numbered layers with configurable sizes
- Layer assignment logic for multi-layer PCBs (F.Cu, In1.Cu, In2.Cu, In3.Cu, In4.Cu, B.Cu)

### Key Design Patterns

**Dimension Parsing**: The `dimension_converter` in main.ml handles multiple unit formats (mm, mil, oz, um) with intelligent defaults, used across all subcommands for consistent UX.

**Multi-Layer Coil Architecture**:

- Coils are generated per-layer with `generate_spiral_segments_layer`
- Even layers (0, 2, 4...) get automatic via generation at segment endpoints
- Odd layers get coordinate mirroring for optimal routing
- Layer assignment follows PCB industry conventions

**Footprint Generation Pipeline**:

1. Generate geometric path segments for each layer
2. Convert segments to KiCad primitives (fp_line, fp_arc)
3. Add pads, vias, properties, and metadata
4. Serialize to S-expression format with custom formatting

## Dependencies

- **OCaml + Dune** - Core build system
- **Cmdliner** - Type-safe command-line parsing
- **sexplib0 + ppx_sexp_conv** - S-expression serialization for KiCad format
- **Uuidm** - UUID generation for KiCad components

## Unit Format Support

All dimensional inputs support flexible unit parsing:

**Length/Width**: `0.5mm`, `20mil`, `1.2` (mm default)
**Thickness**: `35um`, `1oz`, `2oz` (oz default)  
**Via Sizes**: `0.3/0.15`, `12mil/6mil`, `0.4mm/8mil` (copper/drill format)

## Multi-Layer Coil Features

- Supports 1-6 layer coils with automatic layer assignment
- Even layers get thru-hole vias for inter-layer connections
- Odd layers use coordinate mirroring for routing optimization
- Configurable via sizes with separate copper and drill specifications
- Automatic trace clearance and pitch calculations

