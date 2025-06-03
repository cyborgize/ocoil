open Coil
open Sexplib0.Sexp_conv
module Sexp = Sexplib0.Sexp

type kicad_point = {
  x: float;
  y: float;
} [@@deriving sexp_of]

type gr_line = {
  start: kicad_point;
  end_point: kicad_point;
  width: float;
} [@@deriving sexp_of]

type gr_arc = {
  start: kicad_point;
  mid: kicad_point;
  end_point: kicad_point;
  width: float;
} [@@deriving sexp_of]

type kicad_primitive = 
  | GrLine of gr_line
  | GrArc of gr_arc

let sexp_of_kicad_primitive = function
  | GrLine gr_line -> Sexp.List [Sexp.Atom "gr_line"; sexp_of_gr_line gr_line]
  | GrArc gr_arc -> Sexp.List [Sexp.Atom "gr_arc"; sexp_of_gr_arc gr_arc]

type pad_type = Circle | Custom

let sexp_of_pad_type = function
  | Circle -> Sexp.Atom "circle"
  | Custom -> Sexp.Atom "custom"

type pad = {
  name: string;
  pad_type: pad_type;
  at: kicad_point;
  size: kicad_point;
  layers: string list;
  options: string list option;
  primitives: kicad_primitive list option;
} [@@deriving sexp_of]

type footprint = {
  name: string;
  version: int;
  generator: string;
  generator_version: string;
  layer: string;
  pads: pad list;
} [@@deriving sexp_of]

(* Convert millimeters to KiCad units and apply offset *)
let mm_to_kicad_point ?(offset = {x = 0.0; y = 0.0}) (p : point) = 
  { x = (p.x *. 1000.0) -. offset.x; y = (p.y *. 1000.0) -. offset.y }

(* Convert spiral segments to KiCad primitives *)
let segment_to_primitive width_mm offset segment =
  match segment with
  | Line { start; end_point } ->
    GrLine {
      start = mm_to_kicad_point ~offset start;
      end_point = mm_to_kicad_point ~offset end_point;
      width = width_mm;
    }
  | Arc { start; mid; end_point; _ } ->
    GrArc {
      start = mm_to_kicad_point ~offset start;
      mid = mm_to_kicad_point ~offset mid;
      end_point = mm_to_kicad_point ~offset end_point;
      width = width_mm;
    }

(* Generate KiCad primitives from spiral segments *)
let generate_kicad_primitives shape track_width pitch turns is_inner ?(offset = { x = 0.0; y = 0.0 }) () =
  let width_mm = track_width *. 1000.0 in
  let segments = generate_spiral_segments shape pitch turns is_inner in
  List.map (segment_to_primitive width_mm offset) segments



(* Generate footprint structure and write to channel *)
let generate_footprint output_channel shape width pitch turns is_inner =
  let segments = generate_spiral_segments shape pitch turns is_inner in
  let pad_size = width *. 1000.0 in
  
  (* Calculate pad positions from first and last segments *)
  let start_point = match List.hd segments with
    | Line { start; _ } | Arc { start; _ } -> start
  in
  let end_point = match List.rev segments |> List.hd with
    | Line { end_point; _ } | Arc { end_point; _ } -> end_point
  in
  
  (* Convert coordinates to KiCad units *)
  let outer_pad_pos = { x = start_point.x *. 1000.0; y = start_point.y *. 1000.0 } in
  let inner_pad_pos = { x = end_point.x *. 1000.0; y = end_point.y *. 1000.0 } in
  
  (* Generate primitives with coordinates relative to outer pad position *)
  let relative_primitives = generate_kicad_primitives shape width pitch turns is_inner ~offset:outer_pad_pos () in
  
  (* Build footprint structure *)
  let footprint = {
    name = "SpiralCoil";
    version = 20241229;
    generator = "copper_trace";
    generator_version = "1.0";
    layer = "F.Cu";
    pads = [
      {
        name = "1";
        pad_type = Custom;
        at = outer_pad_pos;
        size = { x = pad_size; y = pad_size };
        layers = ["F.Cu"];
        options = Some ["(clearance outline)"; "(anchor circle)"];
        primitives = Some relative_primitives;
      };
      {
        name = "2";
        pad_type = Circle;
        at = inner_pad_pos;
        size = { x = pad_size; y = pad_size };
        layers = ["F.Cu"];
        options = None;
        primitives = None;
      };
    ];
  } in
  
  (* Convert to sexp and write *)
  let sexp = sexp_of_footprint footprint in
  let formatter = Format.formatter_of_out_channel output_channel in
  Sexp.pp_hum formatter sexp;
  Format.pp_print_newline formatter ();
  Format.pp_print_flush formatter ()
