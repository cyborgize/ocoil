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
let diameter_arg =
  let doc = "Diameter of the spiral coil. Formats: <value>mm, <value>mil, or <value> (mm default)" in
  Arg.(required & pos 0 (some length_width_converter) None & info [] ~docv:"DIAMETER" ~doc)

let pitch_arg =
  let doc = "Pitch between turns. Formats: <value>mm, <value>mil, or <value> (mm default)" in
  Arg.(required & pos 1 (some length_width_converter) None & info [] ~docv:"PITCH" ~doc)

let turns_arg =
  let doc = "Number of turns (can be non-integer, e.g., 2.5)" in
  Arg.(required & pos 2 (some float) None & info [] ~docv:"TURNS" ~doc)

let inner_diameter_flag =
  let doc = "Treat diameter as inner diameter (default: outer diameter)" in
  Arg.(value & flag & info ["inner-diameter"] ~doc)

type coil_shape = Round | Square | Rectangular | Oval

let parse_coil_shape s =
  match String.lowercase_ascii s with
  | "round" | "circle" | "circular" -> Ok Round
  | "square" -> Ok Square  
  | "rectangular" | "rect" -> Ok Rectangular
  | "oval" | "elliptical" -> Ok Oval
  | _ -> Error (`Msg "Invalid coil shape. Use: round, square, rectangular, oval")

let coil_shape_converter = Arg.conv (parse_coil_shape, fun ppf s -> 
  let shape_str = match s with
    | Round -> "round"
    | Square -> "square" 
    | Rectangular -> "rectangular"
    | Oval -> "oval"
  in Format.fprintf ppf "%s" shape_str)

let shape_arg =
  let doc = "Coil shape: round, square, rectangular, oval (default: round)" in
  Arg.(value & opt coil_shape_converter Round & info ["s"; "shape"] ~docv:"SHAPE" ~doc)

let width_dim_arg =
  let doc = "Width dimension for rectangular/oval coils. Formats: <value>mm, <value>mil, or <value> (mm default)" in
  Arg.(value & opt (some length_width_converter) None & info ["width-dim"] ~docv:"WIDTH_DIM" ~doc)

let height_dim_arg =
  let doc = "Height dimension for rectangular/oval coils. Formats: <value>mm, <value>mil, or <value> (mm default)" in
  Arg.(value & opt (some length_width_converter) None & info ["height-dim"] ~docv:"HEIGHT_DIM" ~doc)

(* Spiral coil length calculation *)
let calculate_spiral_length shape diameter pitch turns is_inner width_dim height_dim =
  let op = if is_inner then (+.) else (-.) in
  let radial_expansion = 2.0 *. pitch *. turns in
  let other_diameter = op diameter radial_expansion in
  let avg_diameter = (diameter +. other_diameter) *. 0.5 in
  
  let avg_circumference = match shape with
    | Round -> Float.pi *. avg_diameter
    | Square -> 4.0 *. avg_diameter
    | Rectangular ->
      (match width_dim, height_dim with
       | Some w, Some h ->
         let avg_w = (w +. (op w radial_expansion)) *. 0.5 in
         let avg_h = (h +. (op h radial_expansion)) *. 0.5 in  
         2.0 *. (avg_w +. avg_h)
       | _ -> failwith "Width and height dimensions required for rectangular coils")
    | Oval ->
      (match width_dim, height_dim with
       | Some w, Some h ->
         let avg_w = (w +. (op w radial_expansion)) *. 0.5 in
         let avg_h = (h +. (op h radial_expansion)) *. 0.5 in
         let min_dim = min avg_w avg_h in
         let max_dim = max avg_w avg_h in
         let straight_length = max_dim -. min_dim in
         (* Slot-shaped oval: two straight segments + semicircular ends *)
         let semicircular_length = Float.pi *. min_dim in
         2.0 *. straight_length +. semicircular_length
       | _ -> failwith "Width and height dimensions required for oval coils")
  in
  avg_circumference *. turns

(* Trace subcommand *)
let trace_cmd =
  let doc = "Calculate resistance of a copper trace by length" in
  let info = Cmd.info "trace" ~doc in
  let term = Term.(const (fun length width thickness temperature ->
    let resistance = calculate_resistance length width thickness temperature in
    Printf.printf "Copper trace resistance: %.6f Ohms\n" resistance;
    Printf.printf "Length: %.3f mm, Width: %.3f mm, Thickness: %.3f mm, Temperature: %.1f°C\n" 
      (length *. 1000.0) (width *. 1000.0) (thickness *. 1000.0) temperature
  ) $ trace_length_arg $ width_arg $ thickness_arg $ temperature_arg) in
  Cmd.v info term

(* Coil subcommand *)
let coil_cmd =
  let doc = "Calculate resistance of a spiral PCB coil" in
  let info = Cmd.info "coil" ~doc in
  let term = Term.(const (fun diameter pitch turns shape is_inner width_dim height_dim width thickness temperature ->
    let length = calculate_spiral_length shape diameter pitch turns is_inner width_dim height_dim in
    let resistance = calculate_resistance length width thickness temperature in
    Printf.printf "Spiral coil resistance: %.6f Ohms\n" resistance;
    let diameter_type = if is_inner then "inner" else "outer" in
    let shape_str = match shape with
      | Round -> "round"
      | Square -> "square"
      | Rectangular -> "rectangular" 
      | Oval -> "oval"
    in
    Printf.printf "Shape: %s, Diameter (%s): %.3f mm, Pitch: %.3f mm, Turns: %.3f\n" 
      shape_str diameter_type (diameter *. 1000.0) (pitch *. 1000.0) turns;
    (match width_dim, height_dim with
     | Some w, Some h ->
       Printf.printf "Dimensions: %.3f mm × %.3f mm\n" (w *. 1000.0) (h *. 1000.0)
     | _ -> ());
    Printf.printf "Calculated length: %.3f mm\n" (length *. 1000.0);
    Printf.printf "Width: %.3f mm, Thickness: %.3f mm, Temperature: %.1f°C\n" 
      (width *. 1000.0) (thickness *. 1000.0) temperature
  ) $ diameter_arg $ pitch_arg $ turns_arg $ shape_arg $ inner_diameter_flag $ width_dim_arg $ height_dim_arg $ width_arg $ thickness_arg $ temperature_arg) in
  Cmd.v info term

(* Main command group *)
let main_cmd =
  let doc = "Calculate resistance of copper traces and coils" in
  let info = Cmd.info "copper_trace" ~doc in
  Cmd.group info [trace_cmd; coil_cmd]

let () = exit (Cmd.eval main_cmd)