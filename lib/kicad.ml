open Coil

let generate_kicad_primitives shape track_width pitch turns is_inner ?(offset = { x = 0.0; y = 0.0 }) () =
  let mm_to_kicad p = { x = (p.x *. 1000.0) -. offset.x; y = (p.y *. 1000.0) -. offset.y } in
  let width_mm = track_width *. 1000.0 in
  
  let segments = generate_spiral_segments shape pitch turns is_inner in
  
  List.fold_left (fun acc segment ->
    let primitive = match segment with
      | Line { start; end_point } ->
        let start_kicad = mm_to_kicad start in
        let end_kicad = mm_to_kicad end_point in
        Printf.sprintf "            (gr_line (start %.3f %.3f) (end %.3f %.3f) (width %.3f))"
          start_kicad.x start_kicad.y end_kicad.x end_kicad.y width_mm
      | Arc { start; mid; end_point; _ } ->
        let start_kicad = mm_to_kicad start in
        let mid_kicad = mm_to_kicad mid in
        let end_kicad = mm_to_kicad end_point in
        Printf.sprintf "            (gr_arc (start %.3f %.3f) (mid %.3f %.3f) (end %.3f %.3f) (width %.3f))"
          start_kicad.x start_kicad.y mid_kicad.x mid_kicad.y end_kicad.x end_kicad.y width_mm
    in
    primitive :: acc
  ) [] segments |> List.rev

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
  
  (* Generate KiCad footprint with proper header *)
  Printf.fprintf output_channel "(footprint \"SpiralCoil\"\n";
  Printf.fprintf output_channel "    (version 20241229)\n";
  Printf.fprintf output_channel "    (generator \"copper_trace\")\n";
  Printf.fprintf output_channel "    (generator_version \"1.0\")\n";
  Printf.fprintf output_channel "    (layer \"F.Cu\")\n";
  
  (* Outer pad (start of spiral) *)
  let outer_pad_x = start_point.x *. 1000.0 in
  let outer_pad_y = start_point.y *. 1000.0 in
  
  (* Generate primitives with coordinates relative to pad position *)
  let pad_offset = { x = outer_pad_x; y = outer_pad_y } in
  let relative_primitives = generate_kicad_primitives shape width pitch turns is_inner ~offset:pad_offset () in
  
  Printf.fprintf output_channel "    (pad \"1\" smd custom\n";
  Printf.fprintf output_channel "        (at %.3f %.3f)\n" outer_pad_x outer_pad_y;
  Printf.fprintf output_channel "        (size %.3f %.3f)\n" pad_size pad_size;
  Printf.fprintf output_channel "        (layers \"F.Cu\")\n";
  Printf.fprintf output_channel "        (options (clearance outline) (anchor circle))\n";
  Printf.fprintf output_channel "        (primitives\n";
  List.iter (fun primitive -> Printf.fprintf output_channel "%s\n" primitive) relative_primitives;
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
  Printf.fprintf output_channel ")\n"