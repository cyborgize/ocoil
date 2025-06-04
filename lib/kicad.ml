open Coil
open Sexplib0.Sexp_conv
module Sexp = Sexplib0.Sexp

type ('a, 'b) pair = 'a * 'b [@@deriving sexp_of]

let sexp_of_pair sexp_of_a sexp_of_b (a, b) =
  let open Sexp in
  match sexp_of_pair sexp_of_a sexp_of_b (a, b) with
  | List [ List a_sexp; List b_sexp ] -> List (a_sexp @ b_sexp)
  | List [ a_sexp; List b_sexp ] -> List (a_sexp :: b_sexp)
  | List [ List a_sexp; b_sexp ] -> List (a_sexp @ [ b_sexp ])
  | _ -> failwith "bad"

type 'a flatten = 'a [@@deriving sexp]

let sexp_of_flatten sexp_of_a a =
  let open Sexp in
  match sexp_of_a a with
  | List x ->
    List
      (List.concat
         (List.map
            (function
              | List x -> x
              | Atom _ as x -> [ x ])
            x))
  | Atom _ as x -> x

type 'a field = 'a [@@deriving sexp]

let sexp_of_field sexp_of_a a =
  let open Sexp in
  match sexp_of_a a with
  | List [ hd; List tl ] -> List (hd :: tl)
  | (Atom _ | List _) as x -> x

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
    List
      (List.map
         (function
           | List (Atom hd :: tl) when String.ends_with ~suffix:"_" hd ->
             List (Atom (String.sub hd 0 (String.length hd - 1)) :: tl)
           | (List _ | Atom _) as x -> x)
         x)
  | Atom _ as x -> x

type layer =
  | F_Cu
  | B_Cu
  | F_SilkS
  | B_SilkS
  | F_Fab
  | B_Fab
  | F_Mask
  | B_Mask
  | In1_Cu
  | In2_Cu
  | In3_Cu
  | In4_Cu
  | Edge_Cuts
  | Margin
  | F_CrtYd
  | B_CrtYd

let sexp_of_layer = function
  | F_Cu -> Sexp.Atom "F.Cu"
  | B_Cu -> Sexp.Atom "B.Cu"
  | F_SilkS -> Sexp.Atom "F.SilkS"
  | B_SilkS -> Sexp.Atom "B.SilkS"
  | F_Fab -> Sexp.Atom "F.Fab"
  | B_Fab -> Sexp.Atom "B.Fab"
  | F_Mask -> Sexp.Atom "F.Mask"
  | B_Mask -> Sexp.Atom "B.Mask"
  | In1_Cu -> Sexp.Atom "In1.Cu"
  | In2_Cu -> Sexp.Atom "In2.Cu"
  | In3_Cu -> Sexp.Atom "In3.Cu"
  | In4_Cu -> Sexp.Atom "In4.Cu"
  | Edge_Cuts -> Sexp.Atom "Edge.Cuts"
  | Margin -> Sexp.Atom "Margin"
  | F_CrtYd -> Sexp.Atom "F.CrtYd"
  | B_CrtYd -> Sexp.Atom "B.CrtYd"

type gr_coord = float * float [@@deriving sexp_of]

type gr_line' = {
  start : gr_coord;
  end_ : gr_coord;
  width : float;
}
[@@deriving sexp_of]
type gr_line = gr_line' trim fields [@@deriving sexp_of]

type gr_arc' = {
  start : gr_coord;
  mid : gr_coord;
  end_ : gr_coord;
  width : float;
}
[@@deriving sexp_of]
type gr_arc = gr_arc' trim fields [@@deriving sexp_of]

type gr_primitive =
  [ `gr_line of gr_line
  | `gr_arc of gr_arc
  ]
[@@deriving sexp_of]

type pad_coord = float * float [@@deriving sexp_of]
type pad_type = [ `smd ] [@@deriving sexp_of]
type pad_shape =
  [ `circle
  | `custom
  ]
[@@deriving sexp_of]

type pad'' = [ `pad of string * pad_type * pad_shape ] [@@deriving sexp_of]

type pad_options = {
  clearance : [ `outline ];
  anchor : [ `circle ];
}
[@@deriving sexp_of]

type pad' = {
  at : pad_coord;
  size : pad_coord;
  layers : layer list;
  options : pad_options option; [@sexp.option]
  primitives : gr_primitive list fields; [@sexp.omit_nil]
  uuid : string;
}
[@@deriving sexp_of]

type pad = (pad'' field, pad' fields) pair [@@deriving sexp_of]

type property_at = pad_coord [@@deriving sexp_of]
type property_unlocked = [ `yes ] [@@deriving sexp_of]
type property_layer = layer [@@deriving sexp_of]
type property_hide = [ `yes ] [@@deriving sexp_of]
type property_uuid = string [@@deriving sexp_of]

type property_font' = {
  size : float * float;
  thickness : float;
}
[@@deriving sexp_of]

type property_font = property_font' fields [@@deriving sexp_of]

type property_effects' = { font : property_font } [@@deriving sexp_of]

type property_effects = property_effects' fields [@@deriving sexp_of]

type property'' = [ `property of string * string ] [@@deriving sexp_of]

type property' = {
  at : property_at;
  unlocked : property_unlocked option; [@sexp.option]
  layer : property_layer;
  hide : property_hide option; [@sexp.option]
  uuid : property_uuid;
  effects : property_effects;
}
[@@deriving sexp_of]

type property = (property'' field, property' fields) pair [@@deriving sexp_of]

type fp_coord = float * float [@@deriving sexp_of]

type fp_stroke' = {
  width : float;
  type_ : [ `solid ];
}
[@@deriving sexp_of]

type fp_stroke = fp_stroke' trim fields [@@deriving sexp_of]

type fp_rect'' = [ `fp_rect ] [@@deriving sexp_of]

type fp_rect' = {
  start : fp_coord;
  end_ : fp_coord;
  stroke : fp_stroke;
  fill : [ `no ];
  layer : layer;
  uuid : string;
}
[@@deriving sexp_of]

type fp_rect = (fp_rect'' field, fp_rect' trim fields) pair [@@deriving sexp_of]

type fp_line'' = [ `fp_line ] [@@deriving sexp_of]

type fp_line' = {
  start : fp_coord;
  end_ : fp_coord;
  stroke : fp_stroke;
  layer : layer;
  uuid : string;
}
[@@deriving sexp_of]

type fp_line = (fp_line'' field, fp_line' trim fields) pair [@@deriving sexp_of]

type fp_arc'' = [ `fp_arc ] [@@deriving sexp_of]

type fp_arc' = {
  start : fp_coord;
  mid : fp_coord;
  end_ : fp_coord;
  stroke : fp_stroke;
  layer : layer;
  uuid : string;
}
[@@deriving sexp_of]

type fp_arc = (fp_arc'' field, fp_arc' trim fields) pair [@@deriving sexp_of]

type fp_text_coord = float * float * float [@@deriving sexp_of]

type fp_text_font' = {
  size : float * float;
  thickness : float;
}
[@@deriving sexp_of]

type fp_text_font = fp_text_font' fields [@@deriving sexp_of]

type fp_text_effects' = {
  font : fp_text_font;
  justify : [ `mirror ] option; [@sexp.option]
}
[@@deriving sexp_of]

type fp_text_effects = fp_text_effects' fields [@@deriving sexp_of]

type fp_text'' = [ `fp_text of [ `user ] * string ] [@@deriving sexp_of]

type fp_text' = {
  at : fp_text_coord;
  unlocked : [ `yes ] option; [@sexp.option]
  layer : layer;
  uuid : string;
  effects : fp_text_effects;
}
[@@deriving sexp_of]

type fp_text = (fp_text'' field, fp_text' fields) pair [@@deriving sexp_of]

type footprint_meta = {
  version : int;
  generator : string;
  generator_version : string;
  layer : layer;
  attr : [ `smd ];
  embedded_fonts : [ `no ];
}
[@@deriving sexp_of]

type footprint_pads = pad list [@@deriving sexp_of]

type footprint_properties = property list [@@deriving sexp_of]

type footprint_rectangles = fp_rect list [@@deriving sexp_of]

type footprint_lines = fp_line list [@@deriving sexp_of]

type footprint_arcs = fp_arc list [@@deriving sexp_of]

type footprint_texts = fp_text list [@@deriving sexp_of]

type footprint =
  ( [ `footprint of string ],
    ( footprint_meta,
      ( footprint_properties,
        ( footprint_rectangles,
          (footprint_lines, (footprint_arcs, (footprint_texts, footprint_pads) pair) pair) pair )
        pair )
      pair )
    pair )
  pair
[@@deriving sexp_of]

(* Helper function to create a property *)
let create_property name value ~uuid ?(at = 0.0, 0.0) ?(unlocked = Some `yes) ?(layer = F_Fab) ?(hide = Some `yes)
  ?(font_size = 1.0, 1.0) ?(font_thickness = 0.15) () =
  ( `property (name, value),
    { at; unlocked; layer; hide; uuid; effects = { font = { size = font_size; thickness = font_thickness } } } )

(* Helper function to create a pad *)
let create_pad number pad_type pad_shape ~at ~size ~layers ~options ~primitives ~uuid =
  `pad (number, pad_type, pad_shape), { at; size; layers; options; primitives; uuid }

(* Helper function to create a footprint rectangle *)
let create_fp_rect ~start ~end_ ~stroke_width ~stroke_type ~fill ~layer ~uuid =
  `fp_rect, { start; end_; stroke = { width = stroke_width; type_ = stroke_type }; fill; layer; uuid }

(* Helper function to create a footprint line *)
let create_fp_line ~start ~end_ ~stroke_width ~stroke_type ~layer ~uuid =
  `fp_line, { start; end_; stroke = { width = stroke_width; type_ = stroke_type }; layer; uuid }

(* Helper function to create a footprint arc *)
let create_fp_arc ~start ~mid ~end_ ~stroke_width ~stroke_type ~layer ~uuid =
  `fp_arc, { start; mid; end_; stroke = { width = stroke_width; type_ = stroke_type }; layer; uuid }

(* Helper function to create a footprint text *)
let create_fp_text text_content ~at ~layer ~uuid ?(text_type = `user) ?(unlocked = Some `yes) ?(font_size = 1.0, 1.0)
  ?(font_thickness = 0.15) ?(justify = None) () =
  ( `fp_text (text_type, text_content),
    { at; unlocked; layer; uuid; effects = { font = { size = font_size; thickness = font_thickness }; justify } } )

(* Convert spiral segments to KiCad primitives *)
let segment_to_primitive width_mm offset segment =
  match segment with
  | Line { start; end_point } ->
    `gr_line
      {
        start = (start.x *. 1000.0) -. offset.x, (start.y *. 1000.0) -. offset.y;
        end_ = (end_point.x *. 1000.0) -. offset.x, (end_point.y *. 1000.0) -. offset.y;
        width = width_mm;
      }
  | Arc { start; mid; end_point; _ } ->
    `gr_arc
      {
        start = (start.x *. 1000.0) -. offset.x, (start.y *. 1000.0) -. offset.y;
        mid = (mid.x *. 1000.0) -. offset.x, (mid.y *. 1000.0) -. offset.y;
        end_ = (end_point.x *. 1000.0) -. offset.x, (end_point.y *. 1000.0) -. offset.y;
        width = width_mm;
      }

(* Generate KiCad primitives from spiral segments *)
let generate_kicad_primitives ~shape ~track_width ~pitch ~turns ~is_inner ?(offset = { x = 0.0; y = 0.0 }) () =
  let width_mm = track_width *. 1000.0 in
  let segments = generate_spiral_segments ~shape ~pitch ~turns ~is_inner ~trace_width:track_width in
  List.map (segment_to_primitive width_mm offset) segments

(* Generate footprint structure and write to channel *)
let generate_footprint output_channel ~shape ~width ~pitch ~turns ~is_inner ~layers =
  let segments = generate_spiral_segments ~shape ~pitch ~turns ~is_inner ~trace_width:width in
  let pad_size = width *. 1000.0 in

  (* Calculate pad positions from first and last segments *)
  let start_point =
    match List.hd segments with
    | Line { start; _ } | Arc { start; _ } -> start
  in
  let end_point =
    match List.rev segments |> List.hd with
    | Line { end_point; _ } | Arc { end_point; _ } -> end_point
  in

  (* Convert coordinates to KiCad units *)
  let outer_pad_pos = { x = start_point.x *. 1000.0; y = start_point.y *. 1000.0 } in
  let inner_pad_pos = { x = end_point.x *. 1000.0; y = end_point.y *. 1000.0 } in

  (* Generate primitives with coordinates relative to outer pad position *)
  let relative_primitives =
    generate_kicad_primitives ~shape ~track_width:width ~pitch ~turns ~is_inner ~offset:outer_pad_pos ()
  in

  (* Build footprint structure using helper functions *)
  let pad1 =
    create_pad "1" `smd `custom ~at:(outer_pad_pos.x, outer_pad_pos.y) ~size:(pad_size, pad_size) ~layers:[ F_Cu ]
      ~options:(Some { clearance = `outline; anchor = `circle })
      ~primitives:relative_primitives ~uuid:"14a21f80-a77f-4136-92e0-923221b0e518"
  in

  let pad2 =
    create_pad "2" `smd `circle ~at:(inner_pad_pos.x, inner_pad_pos.y) ~size:(pad_size, pad_size) ~layers:[ F_Cu ]
      ~options:None ~primitives:[] ~uuid:"51855533-01a7-4cb4-87b1-27ff621360b1"
  in

  (* Create properties using helper functions *)
  let ref_property =
    create_property "Reference" "L**" ~uuid:"c53f67d9-d280-48cc-b065-55df925e8d56" ~at:(0.0, -0.5) ~layer:F_SilkS
      ~font_thickness:0.1 ()
  in

  let value_property = create_property "Value" "Val**" ~uuid:"dcb50d06-6893-4b51-a6f1-3a9e9428cb83" ~at:(0.0, 1.0) () in

  let datasheet_property = create_property "Datasheet" "" ~uuid:"cf551db9-fc86-4936-b027-051c526e4bcd" () in

  let description_property = create_property "Description" "" ~uuid:"184cda2b-7a4b-4003-af2a-58ca378cc9de" () in

  (* Generate MFR field based on input parameters *)
  let mfr_string =
    let shape_str =
      match shape with
      | Round { diameter } -> Printf.sprintf "%.0fx%.0f" (diameter *. 1000.0) (diameter *. 1000.0)
      | Square { size } -> Printf.sprintf "%.0fx%.0f" (size *. 1000.0) (size *. 1000.0)
      | Rectangular { width; height } -> Printf.sprintf "%.0fx%.0f" (width *. 1000.0) (height *. 1000.0)
      | Oval { width; height } -> Printf.sprintf "%.0fx%.0f" (width *. 1000.0) (height *. 1000.0)
    in
    let width_str = Printf.sprintf "T%.1f" (width *. 1000.0) in
    let pitch_str = Printf.sprintf "P%.1f" (pitch *. 1000.0) in
    let layers_str = Printf.sprintf "S%d" layers in
    Printf.sprintf "Coil_%s_%s_%s_%s" shape_str width_str pitch_str layers_str
  in

  let mfr_property = create_property "MFR" mfr_string ~uuid:"3826364c-19d9-4f33-a074-aa49377a9ce9" ~unlocked:None () in

  (* Generate MFN field based on input parameters *)
  let mfn_string =
    let dimensions_str =
      match shape with
      | Round { diameter } -> Printf.sprintf "%.0f" (diameter *. 1000.0)
      | Square { size } -> Printf.sprintf "%.0f" (size *. 1000.0)
      | Rectangular { width; height } -> Printf.sprintf "%.0fx%.0f" (width *. 1000.0) (height *. 1000.0)
      | Oval { width; height } -> Printf.sprintf "%.0fx%.0f" (width *. 1000.0) (height *. 1000.0)
    in
    let layers_str = Printf.sprintf "S%d" layers in
    let width_hundredths = int_of_float (width *. 1000.0 *. 100.0) in
    (* Convert to hundredths of mm *)
    let pitch_hundredths = int_of_float (pitch *. 1000.0 *. 100.0) in
    (* Convert to hundredths of mm *)
    let turns_tenths = int_of_float (turns *. 10.0) in
    (* Convert to tenths *)
    Printf.sprintf "%s%sW%02dP%02dT%03d" dimensions_str layers_str width_hundredths pitch_hundredths turns_tenths
  in

  let mfn_property =
    create_property "MFN" mfn_string ~uuid:"4554da17-05d2-4bc7-9de7-a5e9cf606005" ~unlocked:None ~hide:None ()
  in

  (* Calculate rectangle dimensions based on coil shape *)
  let base_width, base_height =
    match shape with
    | Round { diameter } -> diameter, diameter
    | Square { size } -> size, size
    | Rectangular { width; height } -> width, height
    | Oval { width; height } -> width, height
  in

  (* Add clearance: pitch already includes trace width *)
  let clearance = pitch in
  let rect_width = (base_width +. clearance) *. 1000.0 in
  (* Convert to mm *)
  let rect_height = (base_height +. clearance) *. 1000.0 in
  (* Convert to mm *)
  let half_width = rect_width /. 2.0 in
  let half_height = rect_height /. 2.0 in

  (* Create footprint rectangle *)
  let fp_rectangle =
    create_fp_rect ~start:(-.half_width, -.half_height) ~end_:(half_width, half_height) ~stroke_width:0.1
      ~stroke_type:`solid ~fill:`no ~layer:F_SilkS ~uuid:"b55377dc-9f66-4417-90d6-e165a7b32bc3"
  in

  (* Create footprint texts *)
  let mfn_text =
    create_fp_text "${MFN}" ~at:(0.0, 0.0, 0.0) ~layer:F_SilkS ~uuid:"222182e3-a584-4cc6-a41b-c064499fe8b3"
      ~font_size:(0.9, 0.9) ~font_thickness:0.1 ()
  in

  let ref_text_back =
    create_fp_text "${REFERENCE}" ~at:(0.0, -2.2125, 180.0) ~layer:B_Fab ~uuid:"424aa4af-1cf4-4d68-8e2b-7f4fed7b06c5"
      ~justify:(Some `mirror) ()
  in

  let ref_text_front =
    create_fp_text "${REFERENCE}" ~at:(0.0, 2.4875, 0.0) ~layer:F_Fab ~uuid:"0439e334-26b8-4223-ace8-f721b97f5b67" ()
  in

  let footprint =
    ( `footprint "SpiralCoil",
      ( {
          version = 20241229;
          generator = "copper_trace";
          generator_version = "1.0 ";
          layer = F_Cu;
          attr = `smd;
          embedded_fonts = `no;
        },
        ( [ ref_property; value_property; datasheet_property; description_property; mfr_property; mfn_property ],
          ([ fp_rectangle ], ([], ([], ([ mfn_text; ref_text_back; ref_text_front ], [ pad1; pad2 ])))) ) ) )
  in

  (* Convert to sexp and write *)
  let sexp = sexp_of_footprint footprint in
  let formatter = Format.formatter_of_out_channel output_channel in
  Sexp.pp_hum formatter sexp;
  Format.pp_print_newline formatter ();
  Format.pp_print_flush formatter ()
