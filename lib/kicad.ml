open Coil
open Sexplib0.Sexp_conv
module Sexp = Sexplib0.Sexp

type ('a, 'b) pair = 'a * 'b [@@deriving sexp_of]

let sexp_of_pair sexp_of_a sexp_of_b (a, b) =
  let open Sexp in
  match sexp_of_pair sexp_of_a sexp_of_b (a, b) with
  | List [List a_sexp; List b_sexp] -> List (a_sexp @ b_sexp)
  | List [a_sexp; List b_sexp] -> List (a_sexp :: b_sexp)
  | List [List a_sexp; b_sexp] -> List (a_sexp @ [b_sexp])
  | _ -> failwith "bad"

type 'a flatten = 'a [@@deriving sexp]

let sexp_of_flatten sexp_of_a a =
  let open Sexp in
  match sexp_of_a a with
  | List x -> List (List.concat (List.map (function List x -> x | Atom _ as x -> [x]) x))
  | Atom _ as x -> x

type 'a field = 'a [@@deriving sexp]

let sexp_of_field sexp_of_a a =
  let open Sexp in
  match sexp_of_a a with
  | List [ hd; List tl; ] -> List (hd :: tl)
  | Atom _ | List _ as x -> x

type 'a fields = 'a [@@deriving sexp]

let sexp_of_fields sexp_of_a a =
  let open Sexp in
  match sexp_of_a a with
  | List x -> List (List.map (sexp_of_field Fun.id) x)
  | Atom _ as x -> x

type 'a trim = 'a [@@deriving sexp]

let sexp_of_trim sexp_of_a a =
  let open Sexp in
  match sexp_of_a a with
  | List x ->
    List (List.map (function
      | List (Atom hd :: tl) when String.ends_with ~suffix:"_" hd ->
        List (Atom (String.sub hd 0 (String.length hd - 1)) :: tl)
      | List _ | Atom _ as x -> x) x)
  | Atom _ as x -> x

type gr_coord = float * float [@@deriving sexp_of]

type gr_line' = { start : gr_coord; end_: gr_coord; width: float; } [@@deriving sexp_of]
type gr_line = gr_line' trim fields [@@deriving sexp_of]

type gr_arc' = { start : gr_coord; mid : gr_coord; end_: gr_coord; width: float; } [@@deriving sexp_of]
type gr_arc = gr_arc' trim fields [@@deriving sexp_of]

type gr_primitive = [ `gr_line of gr_line | `gr_arc of gr_arc ] [@@deriving sexp_of]

type pad_coord = float * float [@@deriving sexp_of]
type pad_type = [ `smd ] [@@deriving sexp_of]
type pad_shape = [ `circle | `custom ] [@@deriving sexp_of]

type kicad_pad'' = [ `pad of string * pad_type * pad_shape ] [@@deriving sexp_of]

type kicad_pad_options = {
  clearance: [ `outline ];
  anchor: [`circle];
} [@@deriving sexp_of]

type kicad_pad' = {
  at : pad_coord;
  size : pad_coord;
  layers : string list;
  options : kicad_pad_options option [@sexp.option];
  primitives : gr_primitive list fields [@sexp.omit_nil];
} [@@deriving sexp_of]

type kicad_pad = (kicad_pad'' field, kicad_pad' fields) pair [@@deriving sexp_of]

type kicad_footprint_meta = {
  version : int;
  generator : string;
  generator_version : string;
  layer : string
} [@@deriving sexp_of]

type kicad_footprint_pads = kicad_pad list [@@deriving sexp_of]

type kicad_footprint = (
    [ `footprint of string ],
    (kicad_footprint_meta, kicad_footprint_pads) pair
  ) pair [@@deriving sexp_of]

(* Convert spiral segments to KiCad primitives *)
let segment_to_primitive width_mm offset segment =
  match segment with
  | Line { start; end_point } ->
    `gr_line {
      start = ((start.x *. 1000.0) -. offset.x, (start.y *. 1000.0) -. offset.y);
      end_ = ((end_point.x *. 1000.0) -. offset.x, (end_point.y *. 1000.0) -. offset.y);
      width = width_mm;
    }
  | Arc { start; mid; end_point; _ } ->
    `gr_arc {
      start = ((start.x *. 1000.0) -. offset.x, (start.y *. 1000.0) -. offset.y);
      mid = ((mid.x *. 1000.0) -. offset.x, (mid.y *. 1000.0) -. offset.y);
      end_ = ((end_point.x *. 1000.0) -. offset.x, (end_point.y *. 1000.0) -. offset.y);
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
  
  (* Build footprint structure using record types *)
  let pad1 = `pad ("1", `smd, `custom), {
    at = (outer_pad_pos.x, outer_pad_pos.y);
    size = (pad_size, pad_size);
    layers = ["F.Cu"];
    options = Some { clearance = `outline; anchor = `circle; };
    primitives = relative_primitives;
  } in
           
  let pad2 = `pad ("2", `smd, `circle), {
    at = (inner_pad_pos.x, inner_pad_pos.y);
    size = (pad_size, pad_size);
    layers = ["F.Cu"];
    options = None;
    primitives = [];
  } in
  
  let footprint =
    `footprint "SpiralCoil",
    (
      {
        version = 20241229;
        generator = "copper_trace";
        generator_version = "1.0 ";
        layer = "F.Cu";
      },
      [pad1; pad2]
    )
  in
  
  (* Convert to sexp and write *)
  let sexp = sexp_of_kicad_footprint footprint in
  let formatter = Format.formatter_of_out_channel output_channel in
  Sexp.pp_hum formatter sexp;
  Format.pp_print_newline formatter ();
  Format.pp_print_flush formatter ()
