open Coil
open Sexplib0.Sexp_conv
module Sexp = Sexplib0.Sexp

type ('a, 'b) pair = 'a * 'b [@@deriving sexp_of]

let sexp_of_pair sexp_of_a sexp_of_b (a, b) =
  let auto_generated = sexp_of_pair sexp_of_a sexp_of_b (a, b) in
  let open Sexp in
  match auto_generated with
  | List [List [a_sexp]; List [b_sexp]]
  | List [a_sexp; List [b_sexp]]
  | List [List [a_sexp]; b_sexp]
  | List [a_sexp; b_sexp] -> List [a_sexp; b_sexp]
  | _ -> auto_generated

type gr_start = [ `start of float * float ] [@@deriving sexp_of]
type gr_end_point = [ `gr_end of float * float ] [@@deriving sexp_of]  
type gr_mid = [ `mid of float * float ] [@@deriving sexp_of]
type gr_width = [ `width of float ] [@@deriving sexp_of]

(* Only need custom function for end since it's a reserved keyword *)
let sexp_of_gr_end_point = function
  | `gr_end (f1, f2) -> Sexp.List [Sexp.Atom "end"; sexp_of_float f1; sexp_of_float f2]

type gr_line = gr_start * gr_end_point * gr_width [@@deriving sexp_of]
type gr_arc = gr_start * gr_mid * gr_end_point * gr_width [@@deriving sexp_of]

type gr_primitive = [ `gr_line of gr_line | `gr_arc of gr_arc ] [@@deriving sexp_of]

type pad_at = [ `at of float * float ] [@@deriving sexp_of]
type pad_size = [ `size of float * float ] [@@deriving sexp_of]
type pad_layers = [ `layers of string list ] [@@deriving sexp_of]
type pad_options = [ `options of string list ] [@@deriving sexp_of] 
type pad_primitives = [ `primitives of gr_primitive list ] [@@deriving sexp_of]

type pad_shape = [ `smd ] [@@deriving sexp_of]
type pad_type = [ `circle | `custom ] [@@deriving sexp_of]

type kicad_pad = [ `pad of string * pad_shape * pad_type * pad_at * pad_size * pad_layers * pad_options * pad_primitives ] [@@deriving sexp_of]

type kicad_footprint = [ `footprint of string * [ `version of int ] * [ `generator of string ] * [ `generator_version of string ] * [ `layer of string ] * kicad_pad list ] [@@deriving sexp_of]

(* Convert spiral segments to KiCad primitives *)
let segment_to_primitive width_mm offset segment =
  match segment with
  | Line { start; end_point } ->
    `gr_line (
      `start ((start.x *. 1000.0) -. offset.x, (start.y *. 1000.0) -. offset.y),
      `gr_end ((end_point.x *. 1000.0) -. offset.x, (end_point.y *. 1000.0) -. offset.y),
      `width width_mm
    )
  | Arc { start; mid; end_point; _ } ->
    `gr_arc (
      `start ((start.x *. 1000.0) -. offset.x, (start.y *. 1000.0) -. offset.y),
      `mid ((mid.x *. 1000.0) -. offset.x, (mid.y *. 1000.0) -. offset.y),
      `gr_end ((end_point.x *. 1000.0) -. offset.x, (end_point.y *. 1000.0) -. offset.y),
      `width width_mm
    )

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
  
  (* Build footprint structure using simplified types *)
  let pad1 = `pad (
    "1",
    `smd,
    `custom,
    `at (outer_pad_pos.x, outer_pad_pos.y),
    `size (pad_size, pad_size),
    `layers ["F.Cu"],
    `options ["(clearance outline)"; "(anchor circle)"],
    `primitives relative_primitives
  ) in
           
  let pad2 = `pad (
    "2",
    `smd,
    `circle,
    `at (inner_pad_pos.x, inner_pad_pos.y),
    `size (pad_size, pad_size),
    `layers ["F.Cu"],
    `options [],
    `primitives []
  ) in
  
  let footprint = `footprint (
    "SpiralCoil",
    `version 20241229,
    `generator "copper_trace",
    `generator_version "1.0",
    `layer "F.Cu",
    [pad1; pad2]
  ) in
  
  (* Convert to sexp and write *)
  let sexp = sexp_of_kicad_footprint footprint in
  let formatter = Format.formatter_of_out_channel output_channel in
  Sexp.pp_hum formatter sexp;
  Format.pp_print_newline formatter ();
  Format.pp_print_flush formatter ()
