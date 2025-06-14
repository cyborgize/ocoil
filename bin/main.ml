open Cmdliner
open Ocoil_lib

let parse_thickness s =
  let len = String.length s in
  if len = 0 then Error (`Msg "Empty thickness value")
  else if String.ends_with ~suffix:"mm" s then (
    let value_str = String.sub s 0 (len - 2) in
    try
      let value = Float.of_string value_str in
      Ok (value *. 1e-3)
    with Failure _ -> Error (`Msg "Invalid number format"))
  else if String.ends_with ~suffix:"um" s then (
    let value_str = String.sub s 0 (len - 2) in
    try
      let value = Float.of_string value_str in
      Ok (value *. 1e-6)
    with Failure _ -> Error (`Msg "Invalid number format"))
  else if String.ends_with ~suffix:"oz" s then (
    let value_str = String.sub s 0 (len - 2) in
    try
      let value = Float.of_string value_str in
      Ok (Trace.oz_per_sqft_to_meters value)
    with Failure _ -> Error (`Msg "Invalid number format"))
  else (
    try
      let value = Float.of_string s in
      Ok (Trace.oz_per_sqft_to_meters value)
    with Failure _ ->
      Error (`Msg "Invalid thickness format. Use: <value>mm, <value>um, <value>oz, or <value> (oz default)"))

let thickness_converter = Arg.conv (parse_thickness, fun ppf t -> Format.fprintf ppf "%.6e" t)

let parse_length_width s =
  let len = String.length s in
  if len = 0 then Error (`Msg "Empty length/width value")
  else if String.ends_with ~suffix:"mm" s then (
    let value_str = String.sub s 0 (len - 2) in
    try
      let value = Float.of_string value_str in
      Ok (value *. 1e-3)
    with Failure _ -> Error (`Msg "Invalid number format"))
  else if String.ends_with ~suffix:"mil" s then (
    let value_str = String.sub s 0 (len - 3) in
    try
      let value = Float.of_string value_str in
      Ok (Trace.mil_to_meters value)
    with Failure _ -> Error (`Msg "Invalid number format"))
  else (
    try
      let value = Float.of_string s in
      Ok (value *. 1e-3)
    with Failure _ -> Error (`Msg "Invalid length/width format. Use: <value>mm, <value>mil, or <value> (mm default)"))

let dimension_converter = Arg.conv (parse_length_width, fun ppf w -> Format.fprintf ppf "%.6e" w)

let via_size_converter = Arg.pair ~sep:'/' dimension_converter dimension_converter

(* Common cmdliner argument definitions *)
let width_arg =
  let doc = "Width of the copper trace. Formats: <value>mm, <value>mil, or <value> (mm default). Default: 1mm" in
  Arg.(value & opt dimension_converter 1e-3 & info [ "w"; "width" ] ~docv:"WIDTH" ~doc)

let thickness_arg =
  let doc =
    "Thickness of the copper trace. Formats: <value>mm, <value>um, <value>oz, or <value> (oz default). Default: 1oz"
  in
  Arg.(
    value & opt thickness_converter (Trace.oz_per_sqft_to_meters 1.0) & info [ "t"; "thickness" ] ~docv:"THICKNESS" ~doc)

let temperature_arg =
  let doc = "Temperature in degrees Celsius (default: 25°C)" in
  Arg.(value & opt float 25.0 & info [ "temp"; "temperature" ] ~docv:"TEMPERATURE" ~doc)

(* Trace subcommand arguments *)
let trace_length_arg =
  let doc = "Length of the copper trace. Formats: <value>mm, <value>mil, or <value> (mm default)" in
  Arg.(required & pos 0 (some dimension_converter) None & info [] ~docv:"LENGTH" ~doc)

(* Coil subcommand arguments *)
let pitch_arg =
  let doc = "Pitch between turns. Formats: <value>mm, <value>mil, or <value> (mm default)" in
  Arg.(required & opt (some dimension_converter) None & info [ "p"; "pitch" ] ~docv:"PITCH" ~doc)

let turns_arg =
  let doc = "Number of turns (can be non-integer, e.g., 2.5)" in
  Arg.(required & opt (some float) None & info [ "n"; "turns" ] ~docv:"TURNS" ~doc)

let round_converter = Arg.conv (parse_length_width, fun ppf d -> Format.fprintf ppf "%.6e" d)
let square_converter = Arg.conv (parse_length_width, fun ppf s -> Format.fprintf ppf "%.6e" s)
let rectangle_converter = Arg.pair ~sep:'x' dimension_converter dimension_converter
let oval_converter = Arg.pair ~sep:'x' dimension_converter dimension_converter

let round_arg =
  let doc = "Round coil with specified diameter. Formats: <value>mm, <value>mil, or <value> (mm default)" in
  Arg.(value & opt (some round_converter) None & info [ "round" ] ~docv:"DIAMETER" ~doc)

let square_arg =
  let doc = "Square coil with specified size. Formats: <value>mm, <value>mil, or <value> (mm default)" in
  Arg.(value & opt (some square_converter) None & info [ "square" ] ~docv:"SIZE" ~doc)

let rectangle_arg =
  let doc = "Rectangular coil with specified dimensions. Format: <width>x<height> (in mm)" in
  Arg.(value & opt (some rectangle_converter) None & info [ "rectangle" ] ~docv:"WIDTHxHEIGHT" ~doc)

let oval_arg =
  let doc = "Oval coil with specified dimensions. Format: <width>x<height> (in mm)" in
  Arg.(value & opt (some oval_converter) None & info [ "oval" ] ~docv:"WIDTHxHEIGHT" ~doc)

let corner_radius_arg =
  let doc =
    "Corner radius for oval coils. Use 0 for rectangular corners. Formats: <value>mm, <value>mil, or <value> (mm \
     default)"
  in
  Arg.(value & opt (some dimension_converter) None & info [ "corner-radius" ] ~docv:"RADIUS" ~doc)

let layers_arg =
  let doc = "Number of PCB layers (default: 1)" in
  Arg.(value & opt int 1 & info [ "layers" ] ~docv:"LAYERS" ~doc)

let keep_layers_arg =
  let doc = "Number of layers to include in footprint output (default: same as --layers)" in
  Arg.(value & opt (some int) None & info [ "keep-layers" ] ~docv:"KEEP_LAYERS" ~doc)

let clearance_arg =
  let doc =
    "Trace clearance (spacing between adjacent traces). Formats: <value>mm, <value>mil, or <value> (mm default). \
     Default: 0.1mm"
  in
  Arg.(value & opt dimension_converter 1e-4 & info [ "c"; "clearance" ] ~docv:"CLEARANCE" ~doc)

let via_size_arg =
  let doc =
    "Via size as copper/drill. Formats: <value>mm, <value>mil, or <value> (mm default). Example: '0.3/0.15' or \
     '12mil/6mil'. Default: 0.5/0.25"
  in
  Arg.(value & opt via_size_converter (0.5e-3, 0.25e-3) & info [ "via-size" ] ~docv:"COPPER/DRILL" ~doc)

let output_arg =
  let doc = "Output file for KiCad footprint (default: stdout, use \"-\" for stdout)" in
  Arg.(value & opt string "-" & info [ "o"; "output" ] ~docv:"FILE" ~doc)

(* Helper function to extract coil shape from command line arguments *)
let get_coil_shape round_opt square_opt rectangle_opt oval_opt corner_radius_opt =
  match round_opt, square_opt, rectangle_opt, oval_opt with
  | Some diameter, None, None, None -> Coil.Round { diameter }
  | None, Some size, None, None -> Coil.Square { size }
  | None, None, Some (width, height), None -> Coil.Rectangular { width; height }
  | None, None, None, Some (width, height) -> Coil.Oval { width; height; corner_radius = corner_radius_opt }
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
    and+ temperature = temperature_arg in
    let resistance = Trace.calculate_resistance length width thickness temperature in
    Printf.printf "Copper trace resistance: %.6f Ohms\n" resistance;
    Printf.printf "Length: %.3f mm, Width: %.3f mm, Thickness: %.3f mm, Temperature: %.1f°C\n" (length *. 1000.0)
      (width *. 1000.0) (thickness *. 1000.0) temperature
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
    and+ corner_radius_opt = corner_radius_arg
    and+ pitch = pitch_arg
    and+ turns = turns_arg
    and+ layers = layers_arg
    and+ width = width_arg
    and+ thickness = thickness_arg
    and+ temperature = temperature_arg in
    let shape = get_coil_shape round_opt square_opt rectangle_opt oval_opt corner_radius_opt in
    let single_layer_length = Coil.calculate_spiral_length ~shape ~pitch ~turns in
    let total_length = single_layer_length *. float_of_int layers in
    let resistance = Trace.calculate_resistance total_length width thickness temperature in
    Printf.printf "Spiral coil resistance: %.6f Ohms\n" resistance;
    let shape_str, size_info =
      match shape with
      | Coil.Round { diameter } -> "round", Printf.sprintf "Diameter (outer): %.3f mm" (diameter *. 1000.0)
      | Coil.Square { size } -> "square", Printf.sprintf "Size (outer): %.3f mm" (size *. 1000.0)
      | Coil.Rectangular { width; height } ->
        "rectangular", Printf.sprintf "Dimensions: %.3f mm × %.3f mm" (width *. 1000.0) (height *. 1000.0)
      | Coil.Oval { width; height; corner_radius = _ } ->
        "oval", Printf.sprintf "Dimensions: %.3f mm × %.3f mm" (width *. 1000.0) (height *. 1000.0)
    in
    Printf.printf "Shape: %s, %s, Pitch: %.3f mm, Turns: %.3f\n" shape_str size_info (pitch *. 1000.0) turns;
    Printf.printf "Layers: %d, Length per layer: %.3f mm, Total length: %.3f mm\n" layers
      (single_layer_length *. 1000.0) (total_length *. 1000.0);
    Printf.printf "Width: %.3f mm, Thickness: %.3f mm, Temperature: %.1f°C\n" (width *. 1000.0) (thickness *. 1000.0)
      temperature
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
    and+ corner_radius_opt = corner_radius_arg
    and+ pitch = pitch_arg
    and+ turns = turns_arg
    and+ layers = layers_arg
    and+ keep_layers_opt = keep_layers_arg
    and+ width = width_arg
    and+ clearance = clearance_arg
    and+ via_size = via_size_arg
    and+ output_file = output_arg in
    let shape = get_coil_shape round_opt square_opt rectangle_opt oval_opt corner_radius_opt in

    (* Determine keep_layers (default to layers if not specified) *)
    let keep_layers = Option.value keep_layers_opt ~default:layers in

    (* Validate keep_layers *)
    if keep_layers > layers then
      failwith (Printf.sprintf "keep-layers (%d) cannot be greater than layers (%d)" keep_layers layers);

    (* Determine output file path *)
    let actual_output_file =
      if output_file = "-" then "-"
      else if Sys.file_exists output_file && Sys.is_directory output_file then (
        let mfn_string = Kicad.generate_mfn_string ~shape ~trace_width:width ~pitch ~turns ~layers:keep_layers in
        Filename.concat output_file ("Coil_" ^ mfn_string ^ ".kicad_mod"))
      else output_file
    in

    (* Determine output channel *)
    let output_channel = if actual_output_file = "-" then stdout else open_out actual_output_file in

    Kicad.generate_footprint output_channel ~shape ~trace_width:width ~pitch ~turns ~total_layers:layers ~keep_layers
      ~clearance ~via_size;

    (* Close file if it's not stdout *)
    if actual_output_file <> "-" then close_out output_channel
  in
  Cmd.v info term

(* Main command group *)
let main_cmd =
  let doc = "Calculate resistance of copper traces and coils" in
  let info = Cmd.info "ocoil" ~doc in
  Cmd.group info [ trace_cmd; coil_cmd; kicad_cmd ]

let () = exit (Cmd.eval main_cmd)
