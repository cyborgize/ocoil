open Coil

let generate_kicad_primitives shape points track_width pitch turns ?(offset = { x = 0.0; y = 0.0 }) () =
  let mm_to_kicad p = { x = (p.x *. 1000.0) -. offset.x; y = (p.y *. 1000.0) -. offset.y } in
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
      
      let start_point = { x = (start_radius *. cos start_angle) -. offset.x; y = (start_radius *. sin start_angle) -. offset.y } in
      let end_point = { x = (end_radius *. cos end_angle) -. offset.x; y = (end_radius *. sin end_angle) -. offset.y } in
      let mid_point = { x = (mid_radius *. cos mid_angle) -. offset.x; y = (mid_radius *. sin mid_angle) -. offset.y } in
      
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

let generate_footprint output_channel shape points width pitch turns =
  (* Calculate pad positions - outer pad at start, inner pad at end *)
  let start_point = points.(0) in
  let end_point = points.(Array.length points - 1) in
  let pad_size = width *. 1000.0 in
  
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
  let relative_primitives = generate_kicad_primitives shape points width pitch turns ~offset:pad_offset () in
  
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