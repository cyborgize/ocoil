open Cmdliner

let copper_resistivity = 1.68e-8 (* Ohm·m at 20°C *)

let calculate_resistance length width thickness =
  let area = width *. thickness in
  let resistance = (copper_resistivity *. length) /. area in
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

let copper_trace_cmd =
  let doc = "Calculate resistance of a copper trace" in
  let info = Cmd.info "copper_trace" ~doc in
  let term = Term.(const (fun length width thickness ->
    let resistance = calculate_resistance length width thickness in
    Printf.printf "Copper trace resistance: %.6e Ohms\n" resistance;
    Printf.printf "Length: %.3e m, Width: %.3e m, Thickness: %.3e m\n" 
      length width thickness
  ) $ length_arg $ width_arg $ thickness_arg) in
  Cmd.v info term

let () = exit (Cmd.eval copper_trace_cmd)