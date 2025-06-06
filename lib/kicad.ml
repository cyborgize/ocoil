open Coil
open Sexplib0.Sexp_conv
module Sexp = Sexplib0.Sexp
module Uuid = Uuidm

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
  | All_Cu

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
  | All_Cu -> Sexp.Atom "*.Cu"

(* Helper function to generate UUID strings *)
let generate_uuid rand_state = Uuid.to_string (Uuid.v4_gen rand_state ())

(* Array of PCB layers from front to back *)
let pcb_layers = [| F_Cu; In1_Cu; In2_Cu; In3_Cu; In4_Cu; B_Cu |]

(* Assign PCB layer based on layer index and total number of layers *)
let assign_coil_layer layer_index _total_layers keep_layers =
  if keep_layers = 1 then
    (* Single layer output always uses F_Cu *)
    F_Cu
  else if layer_index >= keep_layers then
    (* If layer index exceeds keep_layers, don't include this layer *)
    F_Cu (* This shouldn't happen in practice due to filtering *)
  else if layer_index = keep_layers - 1 then
    (* Last kept layer uses B_Cu for multi-layer output *)
    B_Cu
  else if layer_index >= 0 && layer_index < Array.length pcb_layers then
    (* Use layer by index, defaulting to F_Cu if out of bounds *)
    pcb_layers.(layer_index)
  else F_Cu

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
type pad_type =
  [ `smd
  | `thru_hole
  ]
[@@deriving sexp_of]
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
  drill : float option; [@sexp.option]
  layers : layer list;
  remove_unused_layers : [ `no ] option; [@sexp.option]
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
let create_pad number pad_type pad_shape ~at ~size ~layers ~options ~primitives ~uuid ?(drill = None)
  ?(remove_unused_layers = None) () =
  `pad (number, pad_type, pad_shape), { at; size; drill; layers; remove_unused_layers; options; primitives; uuid }

(* Helper function to create a thru-hole pad *)
let create_thru_hole_pad number pad_shape ~at ~size ~drill ~uuid ?(remove_unused_layers = Some `no) () =
  create_pad number `thru_hole pad_shape ~at ~size ~layers:[ All_Cu ] ~options:None ~primitives:[] ~uuid
    ~drill:(Some drill) ~remove_unused_layers ()

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
  | Line { start; end_ } ->
    `gr_line
      {
        start = (start.x *. 1000.0) -. offset.x, (start.y *. 1000.0) -. offset.y;
        end_ = (end_.x *. 1000.0) -. offset.x, (end_.y *. 1000.0) -. offset.y;
        width = width_mm;
      }
  | Arc { start; mid; end_; _ } ->
    `gr_arc
      {
        start = (start.x *. 1000.0) -. offset.x, (start.y *. 1000.0) -. offset.y;
        mid = (mid.x *. 1000.0) -. offset.x, (mid.y *. 1000.0) -. offset.y;
        end_ = (end_.x *. 1000.0) -. offset.x, (end_.y *. 1000.0) -. offset.y;
        width = width_mm;
      }

(* Convert spiral segments to footprint primitives *)
let segment_to_footprint_primitive rand_state width_mm layer segment =
  let uuid = generate_uuid rand_state in
  match segment with
  | Line { start; end_ } ->
    `Line
      (create_fp_line
         ~start:(start.x *. 1000.0, start.y *. 1000.0)
         ~end_:(end_.x *. 1000.0, end_.y *. 1000.0)
         ~stroke_width:width_mm ~stroke_type:`solid ~layer ~uuid)
  | Arc { start; mid; end_; _ } ->
    `Arc
      (create_fp_arc
         ~start:(start.x *. 1000.0, start.y *. 1000.0)
         ~mid:(mid.x *. 1000.0, mid.y *. 1000.0)
         ~end_:(end_.x *. 1000.0, end_.y *. 1000.0)
         ~stroke_width:width_mm ~stroke_type:`solid ~layer ~uuid)

(* Generate MFN string from parameters *)
let generate_mfn_string ~shape ~trace_width ~pitch ~turns ~layers =
  let dimensions_str =
    match shape with
    | Round { diameter } -> Printf.sprintf "%.0f" ((diameter +. trace_width) *. 1000.0)
    | Square { size } -> Printf.sprintf "%.0f" ((size +. trace_width) *. 1000.0)
    | Rectangular { width; height } ->
      Printf.sprintf "%.0f%.0f" ((width +. trace_width) *. 1000.0) ((height +. trace_width) *. 1000.0)
    | Oval { width; height } ->
      Printf.sprintf "%.0f%.0f" ((width +. trace_width) *. 1000.0) ((height +. trace_width) *. 1000.0)
  in
  let layers_str = Printf.sprintf "S%d" layers in
  let width_hundredths = int_of_float (trace_width *. 1000.0 *. 100.0) in
  (* Convert to hundredths of mm *)
  let pitch_hundredths = int_of_float (pitch *. 1000.0 *. 100.0) in
  (* Convert to hundredths of mm *)
  let turns_tenths = int_of_float (turns *. 10.0) in
  (* Convert to tenths *)
  Printf.sprintf "%s%sW%02dP%02dT%03d" dimensions_str layers_str width_hundredths pitch_hundredths turns_tenths

(* Generate footprint structure and write to channel *)
let generate_footprint output_channel ~shape ~trace_width ~pitch ~turns ~total_layers ~keep_layers ~clearance ~via_size
    =
  let rand_state = Random.State.make_self_init () in
  let via_copper_size, _via_drill_size = via_size in
  let segments =
    generate_spiral_segments ~shape ~pitch ~turns ~trace_width ~clearance ~layers:total_layers ~via_copper_size
  in
  let pad_size = trace_width *. 1000.0 in

  (* Get first available first_point for outer pad *)
  let first_point_opt = List.find_map (fun (layer_seg : Coil.layer_segments) -> layer_seg.first_point) segments in

  (* Generate coil segments as footprint primitives (excluding first layer) *)
  let width_mm = trace_width *. 1000.0 in
  let coil_primitives =
    List.concat_map
      (fun { Coil.layer_index; segments; _ } ->
        if layer_index < keep_layers && layer_index > 0 then (
          let layer = assign_coil_layer layer_index total_layers keep_layers in
          List.map (fun segment -> segment_to_footprint_primitive rand_state width_mm layer segment) segments)
        else [])
      segments
  in

  (* Separate lines and arcs *)
  let coil_lines, coil_arcs =
    List.fold_left
      (fun (lines, arcs) primitive ->
        match primitive with
        | `Line line -> line :: lines, arcs
        | `Arc arc -> lines, arc :: arcs)
      ([], []) coil_primitives
  in
  let coil_lines = List.rev coil_lines in
  let coil_arcs = List.rev coil_arcs in

  (* Build footprint structure using helper functions *)
  let outer_pads =
    match first_point_opt with
    | Some start_point ->
      let outer_pad_pos = { x = start_point.x *. 1000.0; y = start_point.y *. 1000.0 } in
      (* Extract first layer segments for pad1 primitives with proper offset *)
      let first_layer_primitives =
        match List.find_opt (fun { Coil.layer_index; _ } -> layer_index = 0) segments with
        | Some { segments = first_segments; _ } ->
          List.map (fun segment -> segment_to_primitive width_mm outer_pad_pos segment) first_segments
        | None -> []
      in
      [
        create_pad "1" `smd `custom ~at:(outer_pad_pos.x, outer_pad_pos.y) ~size:(pad_size, pad_size) ~layers:[ F_Cu ]
          ~options:(Some { clearance = `outline; anchor = `circle })
          ~primitives:first_layer_primitives ~uuid:(generate_uuid rand_state) ();
      ]
    | None -> []
  in

  (* Generate vias for layers with last_point coordinates (only for kept layers) *)
  let via_pads =
    List.filter_map
      (fun { Coil.layer_index; segments = _; first_point = _; last_point } ->
        if layer_index < keep_layers then (
          match last_point with
          | Some point ->
            (* Use the calculated last point coordinates *)
            let via_pos = point.x *. 1000.0, point.y *. 1000.0 in
            let via_number = Printf.sprintf "V%d" layer_index in
            let copper_size, drill_size = via_size in
            let copper_size_mm = copper_size *. 1000.0 in
            let drill_size_mm = drill_size *. 1000.0 in
            Some
              (create_thru_hole_pad via_number `circle ~at:via_pos ~size:(copper_size_mm, copper_size_mm)
                 ~drill:drill_size_mm ~uuid:(generate_uuid rand_state) ())
          | None -> None)
        else None)
      segments
  in

  (* Create properties using helper functions *)
  let ref_property =
    create_property "Reference" "L**" ~uuid:(generate_uuid rand_state) ~at:(0.0, -0.5) ~layer:F_SilkS
      ~font_thickness:0.1 ()
  in

  let value_property = create_property "Value" "Val**" ~uuid:(generate_uuid rand_state) ~at:(0.0, 1.0) () in

  let datasheet_property = create_property "Datasheet" "" ~uuid:(generate_uuid rand_state) () in

  let description_property = create_property "Description" "" ~uuid:(generate_uuid rand_state) () in

  (* Generate MFR field based on input parameters *)
  let mfr_string =
    let shape_str =
      match shape with
      | Round { diameter } -> Printf.sprintf "%.0fx%.0f" ((diameter +. trace_width) *. 1000.0) (diameter *. 1000.0)
      | Square { size } -> Printf.sprintf "%.0fx%.0f" ((size +. trace_width) *. 1000.0) ((size +. trace_width) *. 1000.0)
      | Rectangular { width; height } ->
        Printf.sprintf "%.0fx%.0f" ((width +. trace_width) *. 1000.0) ((height +. trace_width) *. 1000.0)
      | Oval { width; height } ->
        Printf.sprintf "%.0fx%.0f" ((width +. trace_width) *. 1000.0) ((height +. trace_width) *. 1000.0)
    in
    let width_str = Printf.sprintf "T%.1f" (trace_width *. 1000.0) in
    let pitch_str = Printf.sprintf "P%.1f" (pitch *. 1000.0) in
    let layers_str = Printf.sprintf "S%d" keep_layers in
    Printf.sprintf "Coil_%s_%s_%s_%s" shape_str width_str pitch_str layers_str
  in

  let mfr_property = create_property "MFR" mfr_string ~uuid:(generate_uuid rand_state) ~unlocked:None () in

  (* Generate MFN field based on input parameters *)
  let mfn_string = generate_mfn_string ~shape ~trace_width ~pitch ~turns ~layers:keep_layers in

  let mfn_property = create_property "MFN" mfn_string ~uuid:(generate_uuid rand_state) ~unlocked:None ~hide:None () in

  (* Calculate rectangle dimensions based on coil shape *)
  let base_width, base_height =
    match shape with
    | Round { diameter } -> diameter, diameter
    | Square { size } -> size, size
    | Rectangular { width; height } -> width, height
    | Oval { width; height } -> width, height
  in

  (* Add clearance and trace width around the coil *)
  let rect_width = (base_width +. clearance +. trace_width) *. 1000.0 in
  (* Convert to mm *)
  let rect_height = (base_height +. clearance +. trace_width) *. 1000.0 in
  (* Convert to mm *)
  let half_width = rect_width /. 2.0 in
  let half_height = rect_height /. 2.0 in

  (* Create footprint rectangle *)
  let fp_rectangle =
    create_fp_rect ~start:(-.half_width, -.half_height) ~end_:(half_width, half_height) ~stroke_width:0.1
      ~stroke_type:`solid ~fill:`no ~layer:F_SilkS ~uuid:(generate_uuid rand_state)
  in

  (* Create footprint texts *)
  let mfn_text =
    create_fp_text "${MFN}" ~at:(0.0, 0.0, 0.0) ~layer:F_SilkS ~uuid:(generate_uuid rand_state) ~font_size:(0.9, 0.9)
      ~font_thickness:0.1 ()
  in

  let ref_text_back =
    create_fp_text "${REFERENCE}" ~at:(0.0, -2.2125, 180.0) ~layer:B_Fab ~uuid:(generate_uuid rand_state)
      ~justify:(Some `mirror) ()
  in

  let ref_text_front =
    create_fp_text "${REFERENCE}" ~at:(0.0, 2.4875, 0.0) ~layer:F_Fab ~uuid:(generate_uuid rand_state) ()
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
          ( [ fp_rectangle ],
            (coil_lines, (coil_arcs, ([ mfn_text; ref_text_back; ref_text_front ], outer_pads @ via_pads))) ) ) ) )
  in

  (* Convert to sexp and write *)
  let sexp = sexp_of_footprint footprint in
  let formatter = Format.formatter_of_out_channel output_channel in
  Sexp.pp_hum formatter sexp;
  Format.pp_print_newline formatter ();
  Format.pp_print_flush formatter ()
