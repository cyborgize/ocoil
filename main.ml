open Cmdliner

let copper_resistivity_20c = 1.68e-8 (* Ohm·m at 20°C *)
let temperature_coefficient = 0.00393 (* per °C for copper *)

let oz_per_sqft_to_meters oz_per_sqft =
  let oz_to_kg = 0.0283495231 in
  let sqft_to_sqm = 0.09290304 in
  let copper_density = 8960.0 in
  let thickness_kg_per_sqm = (oz_per_sqft *. oz_to_kg) /. sqft_to_sqm in
  thickness_kg_per_sqm /. copper_density

let mil_to_meters mil =
  mil *. 25.4e-6

let parse_thickness s =
  let len = String.length s in
  if len = 0 then
    Error (`Msg "Empty thickness value")
  else if String.ends_with ~suffix:"mm" s then
    let value_str = String.sub s 0 (len - 2) in
    (try
       let value = Float.of_string value_str in
       Ok (value *. 1e-3)
     with
     | Failure _ -> Error (`Msg "Invalid number format"))
  else if String.ends_with ~suffix:"um" s then
    let value_str = String.sub s 0 (len - 2) in
    (try
       let value = Float.of_string value_str in
       Ok (value *. 1e-6)
     with
     | Failure _ -> Error (`Msg "Invalid number format"))
  else if String.ends_with ~suffix:"oz" s then
    let value_str = String.sub s 0 (len - 2) in
    (try
       let value = Float.of_string value_str in
       Ok (oz_per_sqft_to_meters value)
     with
     | Failure _ -> Error (`Msg "Invalid number format"))
  else
    try
      let value = Float.of_string s in
      Ok (oz_per_sqft_to_meters value)
    with
    | Failure _ -> Error (`Msg "Invalid thickness format. Use: <value>mm, <value>um, <value>oz, or <value> (oz default)")

let thickness_converter = Arg.conv (parse_thickness, fun ppf t -> Format.fprintf ppf "%.6e" t)

let parse_length_width s =
  let len = String.length s in
  if len = 0 then
    Error (`Msg "Empty length/width value")
  else if String.ends_with ~suffix:"mm" s then
    let value_str = String.sub s 0 (len - 2) in
    (try
       let value = Float.of_string value_str in
       Ok (value *. 1e-3)
     with
     | Failure _ -> Error (`Msg "Invalid number format"))
  else if String.ends_with ~suffix:"mil" s then
    let value_str = String.sub s 0 (len - 3) in
    (try
       let value = Float.of_string value_str in
       Ok (mil_to_meters value)
     with
     | Failure _ -> Error (`Msg "Invalid number format"))
  else
    try
      let value = Float.of_string s in
      Ok (value *. 1e-3)
    with
    | Failure _ -> Error (`Msg "Invalid length/width format. Use: <value>mm, <value>mil, or <value> (mm default)")

let length_width_converter = Arg.conv (parse_length_width, fun ppf w -> Format.fprintf ppf "%.6e" w)

let calculate_resistivity_at_temp temp_celsius =
  copper_resistivity_20c *. (1.0 +. temperature_coefficient *. (temp_celsius -. 20.0))

let calculate_resistance length width thickness temperature =
  let area = width *. thickness in
  let resistivity = calculate_resistivity_at_temp temperature in
  let resistance = (resistivity *. length) /. area in
  resistance

(* Common cmdliner argument definitions *)
let width_arg =
  let doc = "Width of the copper trace. Formats: <value>mm, <value>mil, or <value> (mm default). Default: 1mm" in
  Arg.(value & opt length_width_converter 1e-3 & info ["w"; "width"] ~docv:"WIDTH" ~doc)

let thickness_arg =
  let doc = "Thickness of the copper trace. Formats: <value>mm, <value>um, <value>oz, or <value> (oz default). Default: 1oz" in
  Arg.(value & opt thickness_converter (oz_per_sqft_to_meters 1.0) & info ["t"; "thickness"] ~docv:"THICKNESS" ~doc)

let temperature_arg =
  let doc = "Temperature in degrees Celsius (default: 25°C)" in
  Arg.(value & opt float 25.0 & info ["temp"; "temperature"] ~docv:"TEMPERATURE" ~doc)

(* Trace subcommand arguments *)
let trace_length_arg =
  let doc = "Length of the copper trace. Formats: <value>mm, <value>mil, or <value> (mm default)" in
  Arg.(required & pos 0 (some length_width_converter) None & info [] ~docv:"LENGTH" ~doc)

(* Coil subcommand arguments *)
(* Shape arguments defined above *)

let pitch_arg =
  let doc = "Pitch between turns. Formats: <value>mm, <value>mil, or <value> (mm default)" in
  Arg.(required & opt (some length_width_converter) None & info ["p"; "pitch"] ~docv:"PITCH" ~doc)

let turns_arg =
  let doc = "Number of turns (can be non-integer, e.g., 2.5)" in
  Arg.(required & opt (some float) None & info ["n"; "turns"] ~docv:"TURNS" ~doc)

let inner_diameter_flag =
  let doc = "Treat diameter as inner diameter (default: outer diameter)" in
  Arg.(value & flag & info ["inner-diameter"] ~doc)

type coil_shape = 
  | Round of { diameter : float }
  | Square of { size : float }
  | Rectangular of { width : float; height : float }
  | Oval of { width : float; height : float }

let round_converter = Arg.conv (parse_length_width, fun ppf d -> Format.fprintf ppf "%.6e" d)
let square_converter = Arg.conv (parse_length_width, fun ppf s -> Format.fprintf ppf "%.6e" s)
let rectangle_converter = Arg.pair ~sep:'x' length_width_converter length_width_converter
let oval_converter = Arg.pair ~sep:'x' length_width_converter length_width_converter

let round_arg =
  let doc = "Round coil with specified diameter. Formats: <value>mm, <value>mil, or <value> (mm default)" in
  Arg.(value & opt (some round_converter) None & info ["round"] ~docv:"DIAMETER" ~doc)

let square_arg =
  let doc = "Square coil with specified size. Formats: <value>mm, <value>mil, or <value> (mm default)" in
  Arg.(value & opt (some square_converter) None & info ["square"] ~docv:"SIZE" ~doc)

let rectangle_arg =
  let doc = "Rectangular coil with specified dimensions. Format: <width>x<height> (in mm)" in
  Arg.(value & opt (some rectangle_converter) None & info ["rectangle"] ~docv:"WIDTHxHEIGHT" ~doc)

let oval_arg =
  let doc = "Oval coil with specified dimensions. Format: <width>x<height> (in mm)" in
  Arg.(value & opt (some oval_converter) None & info ["oval"] ~docv:"WIDTHxHEIGHT" ~doc)

let layers_arg =
  let doc = "Number of PCB layers (default: 1)" in
  Arg.(value & opt int 1 & info ["layers"] ~docv:"LAYERS" ~doc)

(* Spiral coil length calculation *)
let calculate_spiral_length shape pitch turns is_inner =
  let op = if is_inner then (+.) else (-.) in
  let radial_expansion = 2.0 *. pitch *. turns in
  
  let avg_circumference = match shape with
    | Round { diameter } ->
      let other_diameter = op diameter radial_expansion in
      let avg_diameter = (diameter +. other_diameter) *. 0.5 in
      Float.pi *. avg_diameter
    | Square { size } ->
      let other_size = op size radial_expansion in
      let avg_size = (size +. other_size) *. 0.5 in
      4.0 *. avg_size
    | Rectangular { width; height } ->
      let avg_w = (width +. (op width radial_expansion)) *. 0.5 in
      let avg_h = (height +. (op height radial_expansion)) *. 0.5 in  
      2.0 *. (avg_w +. avg_h)
    | Oval { width; height } ->
      let avg_w = (width +. (op width radial_expansion)) *. 0.5 in
      let avg_h = (height +. (op height radial_expansion)) *. 0.5 in
      let min_dim = min avg_w avg_h in
      let max_dim = max avg_w avg_h in
      let straight_length = max_dim -. min_dim in
      (* Slot-shaped oval: two straight segments + semicircular ends *)
      let semicircular_length = Float.pi *. min_dim in
      2.0 *. straight_length +. semicircular_length
  in
  avg_circumference *. turns


(* Trace subcommand *)
let trace_cmd =
  let doc = "Calculate resistance of a copper trace by length" in
  let info = Cmd.info "trace" ~doc in
  let term = 
    let open Cmdliner.Term.Syntax in
    let+ length = trace_length_arg
    and+ width = width_arg
    and+ thickness = thickness_arg
    and+ temperature = temperature_arg
    in
    let resistance = calculate_resistance length width thickness temperature in
    Printf.printf "Copper trace resistance: %.6f Ohms\n" resistance;
    Printf.printf "Length: %.3f mm, Width: %.3f mm, Thickness: %.3f mm, Temperature: %.1f°C\n" 
      (length *. 1000.0) (width *. 1000.0) (thickness *. 1000.0) temperature
  in
  Cmd.v info term

(* Coil subcommand *)
let coil_cmd =
  let doc = "Calculate resistance of a spiral PCB coil" in
  let info = Cmd.info "coil" ~doc in
  let term = 
    let open Cmdliner.Term.Syntax in
    let+ round_opt = round_arg
    and+ square_opt = square_arg
    and+ rectangle_opt = rectangle_arg
    and+ oval_opt = oval_arg
    and+ pitch = pitch_arg
    and+ turns = turns_arg
    and+ is_inner = inner_diameter_flag
    and+ layers = layers_arg
    and+ width = width_arg
    and+ thickness = thickness_arg
    and+ temperature = temperature_arg
    in
    let shape = match round_opt, square_opt, rectangle_opt, oval_opt with
      | Some diameter, None, None, None -> Round { diameter }
      | None, Some size, None, None -> Square { size }
      | None, None, Some (width, height), None -> Rectangular { width; height }
      | None, None, None, Some (width, height) -> Oval { width; height }
      | None, None, None, None -> failwith "Must specify exactly one coil shape: --round, --square, --rectangle, or --oval"
      | _ -> failwith "Must specify exactly one coil shape: --round, --square, --rectangle, or --oval"
    in
    let single_layer_length = calculate_spiral_length shape pitch turns is_inner in
    let total_length = single_layer_length *. (float_of_int layers) in
    let resistance = calculate_resistance total_length width thickness temperature in
    Printf.printf "Spiral coil resistance: %.6f Ohms\n" resistance;
    let diameter_type = if is_inner then "inner" else "outer" in
    let shape_str, size_info = match shape with
      | Round { diameter } -> "round", Printf.sprintf "Diameter (%s): %.3f mm" diameter_type (diameter *. 1000.0)
      | Square { size } -> "square", Printf.sprintf "Size (%s): %.3f mm" diameter_type (size *. 1000.0)
      | Rectangular { width; height } -> "rectangular", Printf.sprintf "Dimensions: %.3f mm × %.3f mm" (width *. 1000.0) (height *. 1000.0)
      | Oval { width; height } -> "oval", Printf.sprintf "Dimensions: %.3f mm × %.3f mm" (width *. 1000.0) (height *. 1000.0)
    in
    Printf.printf "Shape: %s, %s, Pitch: %.3f mm, Turns: %.3f\n" 
      shape_str size_info (pitch *. 1000.0) turns;
    Printf.printf "Layers: %d, Length per layer: %.3f mm, Total length: %.3f mm\n" 
      layers (single_layer_length *. 1000.0) (total_length *. 1000.0);
    Printf.printf "Width: %.3f mm, Thickness: %.3f mm, Temperature: %.1f°C\n" 
      (width *. 1000.0) (thickness *. 1000.0) temperature
  in
  Cmd.v info term

(* KiCad footprint generation *)
type point = { x: float; y: float }

let generate_spiral_path shape pitch turns =
  let steps_per_turn = 64 in
  let total_steps = int_of_float (turns *. float_of_int steps_per_turn) in
  let angle_step = 2.0 *. Float.pi /. float_of_int steps_per_turn in
  
  let points = Array.make (total_steps + 1) { x = 0.0; y = 0.0 } in
  
  for i = 0 to total_steps do
    let angle = float_of_int i *. angle_step in
    let turn_progress = float_of_int i /. float_of_int steps_per_turn in
    let radius_offset = turn_progress *. pitch in
    
    let base_point = match shape with
      | Round { diameter } ->
        let radius = diameter *. 0.5 +. radius_offset in
        { x = radius *. cos angle; y = radius *. sin angle }
      | Square { size } ->
        let half_size = size *. 0.5 +. radius_offset in
        let side_length = 2.0 *. half_size in
        let perimeter = 4.0 *. side_length in
        let pos = (angle /. (2.0 *. Float.pi)) *. perimeter in
        let pos = mod_float pos perimeter in
        if pos < side_length then
          { x = half_size; y = pos -. half_size }
        else if pos < 2.0 *. side_length then
          { x = half_size -. (pos -. side_length); y = half_size }
        else if pos < 3.0 *. side_length then
          { x = -.half_size; y = half_size -. (pos -. 2.0 *. side_length) }
        else
          { x = -.half_size +. (pos -. 3.0 *. side_length); y = -.half_size }
      | Rectangular { width; height } ->
        let half_w = width *. 0.5 +. radius_offset in
        let half_h = height *. 0.5 +. radius_offset in
        let perimeter = 2.0 *. (half_w *. 2.0 +. half_h *. 2.0) in
        let pos = (angle /. (2.0 *. Float.pi)) *. perimeter in
        let pos = mod_float pos perimeter in
        if pos < half_w *. 2.0 then
          { x = half_w; y = pos -. half_w }
        else if pos < half_w *. 2.0 +. half_h *. 2.0 then
          { x = half_w -. (pos -. half_w *. 2.0); y = half_h }
        else if pos < half_w *. 4.0 +. half_h *. 2.0 then
          { x = -.half_w; y = half_h -. (pos -. half_w *. 2.0 -. half_h *. 2.0) }
        else
          { x = -.half_w +. (pos -. half_w *. 4.0 -. half_h *. 2.0); y = -.half_h }
      | Oval { width; height } ->
        let half_w = width *. 0.5 +. radius_offset in
        let half_h = height *. 0.5 +. radius_offset in
        let min_dim = min half_w half_h in
        let max_dim = max half_w half_h in
        let straight_length = max_dim -. min_dim in
        let perimeter = 2.0 *. straight_length +. Float.pi *. min_dim in
        let pos = (angle /. (2.0 *. Float.pi)) *. perimeter in
        let pos = mod_float pos perimeter in
        if width >= height then
          if pos < straight_length then
            { x = half_w; y = pos -. straight_length *. 0.5 }
          else if pos < straight_length +. Float.pi *. min_dim *. 0.5 then
            let arc_angle = (pos -. straight_length) /. min_dim in
            { x = half_w -. min_dim *. (1.0 -. cos arc_angle); y = half_h +. min_dim *. sin arc_angle }
          else if pos < straight_length *. 2.0 +. Float.pi *. min_dim *. 0.5 then
            { x = -.half_w; y = half_h -. (pos -. straight_length -. Float.pi *. min_dim *. 0.5) }
          else
            let arc_angle = (pos -. straight_length *. 2.0 -. Float.pi *. min_dim *. 0.5) /. min_dim in
            { x = -.half_w +. min_dim *. (1.0 -. cos arc_angle); y = -.half_h -. min_dim *. sin arc_angle }
        else
          if pos < straight_length then
            { x = pos -. straight_length *. 0.5; y = half_h }
          else if pos < straight_length +. Float.pi *. min_dim *. 0.5 then
            let arc_angle = (pos -. straight_length) /. min_dim in
            { x = half_w +. min_dim *. sin arc_angle; y = half_h -. min_dim *. (1.0 -. cos arc_angle) }
          else if pos < straight_length *. 2.0 +. Float.pi *. min_dim *. 0.5 then
            { x = half_w -. (pos -. straight_length -. Float.pi *. min_dim *. 0.5); y = -.half_h }
          else
            let arc_angle = (pos -. straight_length *. 2.0 -. Float.pi *. min_dim *. 0.5) /. min_dim in
            { x = -.half_w -. min_dim *. sin arc_angle; y = -.half_h +. min_dim *. (1.0 -. cos arc_angle) }
    in
    points.(i) <- base_point
  done;
  
  points

let generate_kicad_primitives shape points track_width pitch turns =
  let mm_to_kicad p = { x = p.x *. 1000.0; y = p.y *. 1000.0 } in
  let width_mm = track_width *. 1000.0 in
  
  match shape with
  | Round { diameter } ->
    (* For round coils, generate proper spiral arcs *)
    let primitives = ref [] in
    let num_arcs = max 1 (int_of_float (turns *. 4.0)) in (* 4 arcs per turn *)
    let angle_per_arc = (2.0 *. Float.pi *. turns) /. float_of_int num_arcs in
    
    for i = 0 to num_arcs - 1 do
      let start_angle = float_of_int i *. angle_per_arc in
      let end_angle = float_of_int (i + 1) *. angle_per_arc in
      let mid_angle = (start_angle +. end_angle) *. 0.5 in
      
      let start_radius = (diameter *. 0.5 +. pitch *. start_angle /. (2.0 *. Float.pi)) *. 1000.0 in
      let end_radius = (diameter *. 0.5 +. pitch *. end_angle /. (2.0 *. Float.pi)) *. 1000.0 in
      let mid_radius = (diameter *. 0.5 +. pitch *. mid_angle /. (2.0 *. Float.pi)) *. 1000.0 in
      
      let start_point = { x = start_radius *. cos start_angle; y = start_radius *. sin start_angle } in
      let end_point = { x = end_radius *. cos end_angle; y = end_radius *. sin end_angle } in
      let mid_point = { x = mid_radius *. cos mid_angle; y = mid_radius *. sin mid_angle } in
      
      let primitive = Printf.sprintf "            (gr_arc (start %.3f %.3f) (mid %.3f %.3f) (end %.3f %.3f) (width %.3f))"
        start_point.x start_point.y mid_point.x mid_point.y end_point.x end_point.y width_mm in
      primitives := primitive :: !primitives
    done;
    List.rev !primitives
    
  | Oval { width; height } ->
    (* For oval coils, use arcs for rounded ends and lines for straight segments *)
    let primitives = ref [] in
    let min_dim = min width height in
    let is_horizontal = width >= height in
    
    (* Approximate with mixed arcs and lines *)
    for i = 0 to Array.length points - 2 do
      let p1 = mm_to_kicad points.(i) in
      let p2 = mm_to_kicad points.(i + 1) in
      
      (* Determine if this segment should be an arc or line *)
      let is_curved_segment = 
        if is_horizontal then
          abs_float p1.y > (min_dim *. 1000.0 *. 0.3)
        else
          abs_float p1.x > (min_dim *. 1000.0 *. 0.3)
      in
      
      if is_curved_segment && i mod 4 = 0 then
        (* Use arc for curved sections *)
        let mid_x = (p1.x +. p2.x) *. 0.5 in
        let mid_y = (p1.y +. p2.y) *. 0.5 in
        let primitive = Printf.sprintf "            (gr_arc (start %.3f %.3f) (mid %.3f %.3f) (end %.3f %.3f) (width %.3f))"
          p1.x p1.y mid_x mid_y p2.x p2.y width_mm in
        primitives := primitive :: !primitives
      else
        (* Use line for straight sections *)
        let primitive = Printf.sprintf "            (gr_line (start %.3f %.3f) (end %.3f %.3f) (width %.3f))"
          p1.x p1.y p2.x p2.y width_mm in
        primitives := primitive :: !primitives
    done;
    List.rev !primitives
    
  | Square { size = _ } | Rectangular { width = _; height = _ } ->
    (* For square and rectangular coils, use lines only *)
    let primitives = ref [] in
    
    for i = 0 to Array.length points - 2 do
      let p1 = mm_to_kicad points.(i) in
      let p2 = mm_to_kicad points.(i + 1) in
      let primitive = Printf.sprintf "            (gr_line (start %.3f %.3f) (end %.3f %.3f) (width %.3f))" 
        p1.x p1.y p2.x p2.y width_mm in
      primitives := primitive :: !primitives
    done;
    
    List.rev !primitives

let output_arg =
  let doc = "Output file for KiCad footprint (default: stdout, use \"-\" for stdout)" in
  Arg.(value & opt string "-" & info ["o"; "output"] ~docv:"FILE" ~doc)

(* KiCad subcommand *)
let kicad_cmd =
  let doc = "Generate KiCad footprint pad definition for a spiral coil" in
  let info = Cmd.info "kicad" ~doc in
  let term = 
    let open Cmdliner.Term.Syntax in
    let+ round_opt = round_arg
    and+ square_opt = square_arg
    and+ rectangle_opt = rectangle_arg
    and+ oval_opt = oval_arg
    and+ pitch = pitch_arg
    and+ turns = turns_arg
    and+ width = width_arg
    and+ output_file = output_arg
    in
    let shape = match round_opt, square_opt, rectangle_opt, oval_opt with
      | Some diameter, None, None, None -> Round { diameter }
      | None, Some size, None, None -> Square { size }
      | None, None, Some (width, height), None -> Rectangular { width; height }
      | None, None, None, Some (width, height) -> Oval { width; height }
      | None, None, None, None -> failwith "Must specify exactly one coil shape: --round, --square, --rectangle, or --oval"
      | _ -> failwith "Must specify exactly one coil shape: --round, --square, --rectangle, or --oval"
    in
    
    let points = generate_spiral_path shape pitch turns in
    let primitives = generate_kicad_primitives shape points width pitch turns in
    
    (* Calculate pad positions - outer pad at start, inner pad at end *)
    let start_point = points.(0) in
    let end_point = points.(Array.length points - 1) in
    let pad_size = width *. 1000.0 in
    
    (* Determine output channel *)
    let output_channel = 
      if output_file = "-" then
        stdout
      else
        open_out output_file
    in
    
    (* Generate KiCad footprint with proper header *)
    Printf.fprintf output_channel "(footprint \"SpiralCoil\"\n";
    Printf.fprintf output_channel "    (version 20241229)\n";
    Printf.fprintf output_channel "    (generator \"copper_trace\")\n";
    Printf.fprintf output_channel "    (generator_version \"1.0\")\n";
    Printf.fprintf output_channel "    (layer \"F.Cu\")\n";
    
    (* Outer pad (start of spiral) *)
    let outer_pad_x = start_point.x *. 1000.0 in
    let outer_pad_y = start_point.y *. 1000.0 in
    
    Printf.fprintf output_channel "    (pad \"1\" smd custom\n";
    Printf.fprintf output_channel "        (at %.3f %.3f)\n" outer_pad_x outer_pad_y;
    Printf.fprintf output_channel "        (size %.3f %.3f)\n" pad_size pad_size;
    Printf.fprintf output_channel "        (layers \"F.Cu\")\n";
    Printf.fprintf output_channel "        (options (clearance outline) (anchor circle))\n";
    Printf.fprintf output_channel "        (primitives\n";
    List.iter (fun primitive -> Printf.fprintf output_channel "%s\n" primitive) primitives;
    Printf.fprintf output_channel "        )\n";
    Printf.fprintf output_channel "    )\n";
    
    (* Inner pad (end of spiral) *)
    let inner_pad_x = end_point.x *. 1000.0 in
    let inner_pad_y = end_point.y *. 1000.0 in
    
    Printf.fprintf output_channel "    (pad \"2\" smd circle\n";
    Printf.fprintf output_channel "        (at %.3f %.3f)\n" inner_pad_x inner_pad_y;
    Printf.fprintf output_channel "        (size %.3f %.3f)\n" pad_size pad_size;
    Printf.fprintf output_channel "        (layers \"F.Cu\")\n";
    Printf.fprintf output_channel "    )\n";
    Printf.fprintf output_channel ")\n";
    
    (* Close file if it's not stdout *)
    if output_file <> "-" then
      close_out output_channel
  in
  Cmd.v info term

(* Main command group *)
let main_cmd =
  let doc = "Calculate resistance of copper traces and coils" in
  let info = Cmd.info "copper_trace" ~doc in
  Cmd.group info [trace_cmd; coil_cmd; kicad_cmd]

let () = exit (Cmd.eval main_cmd)