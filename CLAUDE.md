# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This directory contains an OCaml command-line tool for calculating the electrical resistance of copper traces, designed for PCB (Printed Circuit Board) design calculations. The tool accounts for temperature effects and supports multiple unit formats commonly used in electronics engineering.

## Build and Development Commands

### Building
```bash
dune build
```

### Running the tool
```bash
dune exec ./main.exe -- <length> [options]
```

### Examples
```bash
# Basic usage with 10mm trace length
dune exec ./main.exe -- 0.01

# Custom width and thickness with units
dune exec ./main.exe -- 0.01 -w 0.5mm -t 2oz

# High temperature calculation
dune exec ./main.exe -- 0.01 --temp 85

# Using mil (thousandths of inch) for width
dune exec ./main.exe -- 0.01 -w 20mil
```

## Code Architecture

### Core Components

**main.ml** - Single file containing the complete application with these key sections:

1. **Physical Constants**: Copper resistivity at 20°C, temperature coefficient, and density
2. **Unit Conversion Functions**: 
   - `oz_per_sqm_to_meters` - Converts copper weight (oz/m²) to thickness in meters
   - `mil_to_meters` - Converts mils to meters (1 mil = 25.4 μm)
3. **Custom Cmdliner Converters**:
   - `thickness_converter` - Parses thickness with units: mm, um, oz, or bare numbers (oz default)
   - `width_converter` - Parses width with units: mm, mil, or bare numbers (mm default)
4. **Resistance Calculation**:
   - `calculate_resistivity_at_temp` - Applies temperature compensation using linear coefficient
   - `calculate_resistance` - Core R = ρL/A calculation

### Dependencies

- **OCaml** - Programming language
- **Dune** - Build system
- **Cmdliner** - Command-line argument parsing with type safety

### Key Design Decisions

- Uses only OCaml Stdlib (no Base library)
- Custom cmdliner converters provide intuitive unit parsing for PCB designers
- Temperature compensation uses standard copper temperature coefficient (0.393%/°C)
- Defaults chosen for common PCB scenarios: 1oz copper thickness, 1mm width, 25°C temperature

## Unit Formats

### Thickness
- `35um` or `0.035mm` - Micrometers or millimeters  
- `1oz` or `2oz` - Copper weight in ounces per square meter
- `1.5` - Bare number defaults to oz

### Width  
- `0.5mm` - Millimeters
- `20mil` - Mils (thousandths of an inch)
- `1.2` - Bare number defaults to mm

### Temperature
- Always in degrees Celsius
- Accounts for copper's positive temperature coefficient