open Cmdliner

let copper_resistivity_20c = 1.68e-8 (* Ohm·m at 20°C *)
let temperature_coefficient = 0.00393 (* per °C for copper *)

let calculate_resistivity_at_temp temp_celsius =
  copper_resistivity_20c *. (1.0 +. temperature_coefficient *. (temp_celsius -. 20.0))

let calculate_resistance length width thickness temperature =
  let area = width *. thickness in
  let resistivity = calculate_resistivity_at_temp temperature in
  let resistance = (resistivity *. length) /. area in
  resistance

let length_arg =
  let doc = "Length of the copper trace in meters" in
  Arg.(required & pos 0 (some float) None & info [] ~docv:"LENGTH" ~doc)

let width_arg =
  let doc = "Width of the copper trace in meters" in
  Arg.(value & opt float 1e-3 & info ["w"; "width"] ~docv:"WIDTH" ~doc)

let thickness_arg =
  let doc = "Thickness of the copper trace in meters (default: 35μm)" in
  Arg.(value & opt float 35e-6 & info ["t"; "thickness"] ~docv:"THICKNESS" ~doc)

let temperature_arg =
  let doc = "Temperature in degrees Celsius (default: 25°C)" in
  Arg.(value & opt float 25.0 & info ["temp"; "temperature"] ~docv:"TEMPERATURE" ~doc)

let copper_trace_cmd =
  let doc = "Calculate resistance of a copper trace" in
  let info = Cmd.info "copper_trace" ~doc in
  let term = Term.(const (fun length width thickness temperature ->
    let resistance = calculate_resistance length width thickness temperature in
    Printf.printf "Copper trace resistance: %.6e Ohms\n" resistance;
    Printf.printf "Length: %.3e m, Width: %.3e m, Thickness: %.3e m, Temperature: %.1f°C\n" 
      length width thickness temperature
  ) $ length_arg $ width_arg $ thickness_arg $ temperature_arg) in
  Cmd.v info term

let () = exit (Cmd.eval copper_trace_cmd)