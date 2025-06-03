open Cmdliner
open Copper_trace_lib

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
       Ok (Trace.oz_per_sqft_to_meters value)
     with
     | Failure _ -> Error (`Msg "Invalid number format"))
  else
    try
      let value = Float.of_string s in
      Ok (Trace.oz_per_sqft_to_meters value)
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
       Ok (Trace.mil_to_meters value)
     with
     | Failure _ -> Error (`Msg "Invalid number format"))
  else
    try
      let value = Float.of_string s in
      Ok (value *. 1e-3)
    with
    | Failure _ -> Error (`Msg "Invalid length/width format. Use: <value>mm, <value>mil, or <value> (mm default)")

let length_width_converter = Arg.conv (parse_length_width, fun ppf w -> Format.fprintf ppf "%.6e" w)

(* Common cmdliner argument definitions *)
let width_arg =
  let doc = "Width of the copper trace. Formats: <value>mm, <value>mil, or <value> (mm default). Default: 1mm" in
  Arg.(value & opt length_width_converter 1e-3 & info ["w"; "width"] ~docv:"WIDTH" ~doc)

let thickness_arg =
  let doc = "Thickness of the copper trace. Formats: <value>mm, <value>um, <value>oz, or <value> (oz default). Default: 1oz" in
  Arg.(value & opt thickness_converter (Trace.oz_per_sqft_to_meters 1.0) & info ["t"; "thickness"] ~docv:"THICKNESS" ~doc)

let temperature_arg =
  let doc = "Temperature in degrees Celsius (default: 25°C)" in
  Arg.(value & opt float 25.0 & info ["temp"; "temperature"] ~docv:"TEMPERATURE" ~doc)

(* Trace subcommand arguments *)
let trace_length_arg =
  let doc = "Length of the copper trace. Formats: <value>mm, <value>mil, or <value> (mm default)" in
  Arg.(required & pos 0 (some length_width_converter) None & info [] ~docv:"LENGTH" ~doc)

(* Coil subcommand arguments *)
let pitch_arg =
  let doc = "Pitch between turns. Formats: <value>mm, <value>mil, or <value> (mm default)" in
  Arg.(required & opt (some length_width_converter) None & info ["p"; "pitch"] ~docv:"PITCH" ~doc)

let turns_arg =
  let doc = "Number of turns (can be non-integer, e.g., 2.5)" in
  Arg.(required & opt (some float) None & info ["n"; "turns"] ~docv:"TURNS" ~doc)

let inner_diameter_flag =
  let doc = "Treat diameter as inner diameter (default: outer diameter)" in
  Arg.(value & flag & info ["inner-diameter"] ~doc)

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

let output_arg =
  let doc = "Output file for KiCad footprint (default: stdout, use \"-\" for stdout)" in
  Arg.(value & opt string "-" & info ["o"; "output"] ~docv:"FILE" ~doc)

(* Helper function to extract coil shape from command line arguments *)
let get_coil_shape round_opt square_opt rectangle_opt oval_opt =
  match round_opt, square_opt, rectangle_opt, oval_opt with
  | Some diameter, None, None, None -> Coil.Round { diameter }
  | None, Some size, None, None -> Coil.Square { size }
  | None, None, Some (width, height), None -> Coil.Rectangular { width; height }
  | None, None, None, Some (width, height) -> Coil.Oval { width; height }
  | None, None, None, None -> failwith "Must specify exactly one coil shape: --round, --square, --rectangle, or --oval"
  | _ -> failwith "Must specify exactly one coil shape: --round, --square, --rectangle, or --oval"

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
    let resistance = Trace.calculate_resistance length width thickness temperature in
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
    let shape = get_coil_shape round_opt square_opt rectangle_opt oval_opt in
    let single_layer_length = Coil.calculate_spiral_length shape pitch turns is_inner in
    let total_length = single_layer_length *. (float_of_int layers) in
    let resistance = Trace.calculate_resistance total_length width thickness temperature in
    Printf.printf "Spiral coil resistance: %.6f Ohms\n" resistance;
    let diameter_type = if is_inner then "inner" else "outer" in
    let shape_str, size_info = match shape with
      | Coil.Round { diameter } -> "round", Printf.sprintf "Diameter (%s): %.3f mm" diameter_type (diameter *. 1000.0)
      | Coil.Square { size } -> "square", Printf.sprintf "Size (%s): %.3f mm" diameter_type (size *. 1000.0)
      | Coil.Rectangular { width; height } -> "rectangular", Printf.sprintf "Dimensions: %.3f mm × %.3f mm" (width *. 1000.0) (height *. 1000.0)
      | Coil.Oval { width; height } -> "oval", Printf.sprintf "Dimensions: %.3f mm × %.3f mm" (width *. 1000.0) (height *. 1000.0)
    in
    Printf.printf "Shape: %s, %s, Pitch: %.3f mm, Turns: %.3f\n" 
      shape_str size_info (pitch *. 1000.0) turns;
    Printf.printf "Layers: %d, Length per layer: %.3f mm, Total length: %.3f mm\n" 
      layers (single_layer_length *. 1000.0) (total_length *. 1000.0);
    Printf.printf "Width: %.3f mm, Thickness: %.3f mm, Temperature: %.1f°C\n" 
      (width *. 1000.0) (thickness *. 1000.0) temperature
  in
  Cmd.v info term

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
    and+ is_inner = inner_diameter_flag
    and+ layers = layers_arg
    and+ width = width_arg
    and+ output_file = output_arg
    in
    let shape = get_coil_shape round_opt square_opt rectangle_opt oval_opt in
    
    (* Determine output channel *)
    let output_channel = 
      if output_file = "-" then
        stdout
      else
        open_out output_file
    in
    
    Kicad.generate_footprint output_channel shape width pitch turns is_inner layers;
    
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