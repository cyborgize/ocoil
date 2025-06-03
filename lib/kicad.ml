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
  uuid : string;
} [@@deriving sexp_of]

type kicad_pad = (kicad_pad'' field, kicad_pad' fields) pair [@@deriving sexp_of]

type property_at = pad_coord [@@deriving sexp_of]
type property_unlocked = [ `yes ] [@@deriving sexp_of]
type property_layer = string [@@deriving sexp_of]
type property_hide = [ `yes ] [@@deriving sexp_of] 
type property_uuid = string [@@deriving sexp_of]

type property_font' = {
  size : float * float;
  thickness : float;
} [@@deriving sexp_of]

type property_font = property_font' fields [@@deriving sexp_of]

type property_effects' = {
  font : property_font;
} [@@deriving sexp_of]

type property_effects = property_effects' fields [@@deriving sexp_of]

type kicad_property'' = [ `property of string * string ] [@@deriving sexp_of]

type kicad_property' = {
  at : property_at;
  unlocked : property_unlocked option [@sexp.option];
  layer : property_layer;
  hide : property_hide option [@sexp.option];
  uuid : property_uuid;
  effects : property_effects;
} [@@deriving sexp_of]

type kicad_property = (kicad_property'' field, kicad_property' fields) pair [@@deriving sexp_of]

type fp_rect_coord = float * float [@@deriving sexp_of]

type fp_rect_stroke' = {
  width : float;
  type_ : [ `solid ];
} [@@deriving sexp_of]

type fp_rect_stroke = fp_rect_stroke' trim fields [@@deriving sexp_of]

type fp_rect'' = [ `fp_rect ] [@@deriving sexp_of]

type fp_rect' = {
  start : fp_rect_coord;
  end_ : fp_rect_coord;
  stroke : fp_rect_stroke;
  fill : [ `no ];
  layer : string;
  uuid : string;
} [@@deriving sexp_of]

type fp_rect = (fp_rect'' field, fp_rect' trim fields) pair [@@deriving sexp_of]

type fp_text_coord = float * float * float [@@deriving sexp_of]

type fp_text_font' = {
  size : float * float;
  thickness : float;
} [@@deriving sexp_of]

type fp_text_font = fp_text_font' fields [@@deriving sexp_of]

type fp_text_effects' = {
  font : fp_text_font;
  justify : [ `mirror ] option [@sexp.option];
} [@@deriving sexp_of]

type fp_text_effects = fp_text_effects' fields [@@deriving sexp_of]

type fp_text'' = [ `fp_text of [ `user ] * string ] [@@deriving sexp_of]

type fp_text' = {
  at : fp_text_coord;
  unlocked : [ `yes ] option [@sexp.option];
  layer : string;
  uuid : string;
  effects : fp_text_effects;
} [@@deriving sexp_of]

type fp_text = (fp_text'' field, fp_text' fields) pair [@@deriving sexp_of]

type kicad_footprint_meta = {
  version : int;
  generator : string;
  generator_version : string;
  layer : string;
  attr : [ `smd ];
  embedded_fonts : [ `no ];
} [@@deriving sexp_of]

type kicad_footprint_pads = kicad_pad list [@@deriving sexp_of]

type kicad_footprint_properties = kicad_property list [@@deriving sexp_of]

type kicad_footprint_rectangles = fp_rect list [@@deriving sexp_of]

type kicad_footprint_texts = fp_text list [@@deriving sexp_of]

type kicad_footprint = (
    [ `footprint of string ],
    (
      kicad_footprint_meta,
      (
        kicad_footprint_properties,
        (
          kicad_footprint_rectangles,
          (
            kicad_footprint_texts,
            kicad_footprint_pads
          ) pair
        ) pair
      ) pair
    ) pair
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
    uuid = "14a21f80-a77f-4136-92e0-923221b0e518";
  } in
           
  let pad2 = `pad ("2", `smd, `circle), {
    at = (inner_pad_pos.x, inner_pad_pos.y);
    size = (pad_size, pad_size);
    layers = ["F.Cu"];
    options = None;
    primitives = [];
    uuid = "51855533-01a7-4cb4-87b1-27ff621360b1";
  } in
  
  (* Create properties *)
  let ref_property = `property ("Reference", "L**"), {
    at = (0.0, -0.5);
    unlocked = Some `yes;
    layer = "F.SilkS";
    hide = Some `yes;
    uuid = "c53f67d9-d280-48cc-b065-55df925e8d56";
    effects = { font = { size = (1.0, 1.0); thickness = 0.1 }; };
  } in
  
  let value_property = `property ("Value", "Val**"), {
    at = (0.0, 1.0);
    unlocked = Some `yes;
    layer = "F.Fab";
    hide = Some `yes;
    uuid = "dcb50d06-6893-4b51-a6f1-3a9e9428cb83";
    effects = { font = { size = (1.0, 1.0); thickness = 0.15 }; };
  } in
  
  let datasheet_property = `property ("Datasheet", ""), {
    at = (0.0, 0.0);
    unlocked = Some `yes;
    layer = "F.Fab";
    hide = Some `yes;
    uuid = "cf551db9-fc86-4936-b027-051c526e4bcd";
    effects = { font = { size = (1.0, 1.0); thickness = 0.15 }; };
  } in
  
  let description_property = `property ("Description", ""), {
    at = (0.0, 0.0);
    unlocked = Some `yes;
    layer = "F.Fab";
    hide = Some `yes;
    uuid = "184cda2b-7a4b-4003-af2a-58ca378cc9de";
    effects = { font = { size = (1.0, 1.0); thickness = 0.15 }; };
  } in
  
  let mfr_property = `property ("MFR", "Coil_15x7_T0.5_P0.6_2SA"), {
    at = (0.0, 0.0);
    unlocked = None;
    layer = "F.Fab";
    hide = Some `yes;
    uuid = "3826364c-19d9-4f33-a074-aa49377a9ce9";
    effects = { font = { size = (1.0, 1.0); thickness = 0.15 }; };
  } in
  
  let mfn_property = `property ("MFN", "157S2W50P60T050A"), {
    at = (0.0, 0.0);
    unlocked = None;
    layer = "F.Fab";
    hide = None;
    uuid = "4554da17-05d2-4bc7-9de7-a5e9cf606005";
    effects = { font = { size = (1.0, 1.0); thickness = 0.15 }; };
  } in
  
  (* Create footprint rectangle *)
  let fp_rectangle = `fp_rect, {
    start = (-7.5, -3.5);
    end_ = (7.5, 3.5);
    stroke = { width = 0.1; type_ = `solid; };
    fill = `no;
    layer = "F.SilkS";
    uuid = "b55377dc-9f66-4417-90d6-e165a7b32bc3";
  } in
  
  (* Create footprint texts *)
  let mfn_text = `fp_text (`user, "${MFN}"), {
    at = (0.0, 0.0, 0.0);
    unlocked = Some `yes;
    layer = "F.SilkS";
    uuid = "222182e3-a584-4cc6-a41b-c064499fe8b3";
    effects = { font = { size = (0.9, 0.9); thickness = 0.1 }; justify = None; };
  } in
  
  let ref_text_back = `fp_text (`user, "${REFERENCE}"), {
    at = (0.0, -2.2125, 180.0);
    unlocked = Some `yes;
    layer = "B.Fab";
    uuid = "424aa4af-1cf4-4d68-8e2b-7f4fed7b06c5";
    effects = { font = { size = (1.0, 1.0); thickness = 0.15 }; justify = Some `mirror; };
  } in
  
  let ref_text_front = `fp_text (`user, "${REFERENCE}"), {
    at = (0.0, 2.4875, 0.0);
    unlocked = Some `yes;
    layer = "F.Fab";
    uuid = "0439e334-26b8-4223-ace8-f721b97f5b67";
    effects = { font = { size = (1.0, 1.0); thickness = 0.15 }; justify = None; };
  } in
  
  let footprint =
    `footprint "SpiralCoil",
    (
      {
        version = 20241229;
        generator = "copper_trace";
        generator_version = "1.0 ";
        layer = "F.Cu";
        attr = `smd;
        embedded_fonts = `no;
      },
      ([ref_property; value_property; datasheet_property; description_property; mfr_property; mfn_property], ([fp_rectangle], ([mfn_text; ref_text_back; ref_text_front], [pad1; pad2])))
    )
  in
  
  (* Convert to sexp and write *)
  let sexp = sexp_of_kicad_footprint footprint in
  let formatter = Format.formatter_of_out_channel output_channel in
  Sexp.pp_hum formatter sexp;
  Format.pp_print_newline formatter ();
  Format.pp_print_flush formatter ()
