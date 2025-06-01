type coil_shape = 
  | Round of { diameter : float }
  | Square of { size : float }
  | Rectangular of { width : float; height : float }
  | Oval of { width : float; height : float }

type point = { x: float; y: float }

type path_segment =
  | Line of { start: point; end_point: point }
  | Arc of { start: point; mid: point; end_point: point; radius: float }

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

let generate_round_loop radius turn_number pitch is_inner =
  let op = if is_inner then (+.) else (-.) in
  let angle_offset = Float.pi *. 2.0 *. turn_number in
  let expanded_radius = op radius (pitch *. turn_number) in
  let num_arcs = 8 in
  let angle_step = Float.pi *. 2.0 /. float_of_int num_arcs in
  
  List.init num_arcs (fun i ->
    let start_angle = angle_offset +. float_of_int i *. angle_step in
    let end_angle = angle_offset +. float_of_int (i + 1) *. angle_step in
    let mid_angle = (start_angle +. end_angle) *. 0.5 in
    
    let start_point = { x = expanded_radius *. cos start_angle; y = expanded_radius *. sin start_angle } in
    let end_point = { x = expanded_radius *. cos end_angle; y = expanded_radius *. sin end_angle } in
    let mid_point = { x = expanded_radius *. cos mid_angle; y = expanded_radius *. sin mid_angle } in
    
    Arc { start = start_point; mid = mid_point; end_point; radius = expanded_radius }
  )

let generate_square_loop size turn_number pitch is_inner =
  let op = if is_inner then (+.) else (-.) in
  let expanded_half_size = op (size *. 0.5) (pitch *. turn_number) in
  [
    Line { start = { x = expanded_half_size; y = -.expanded_half_size }; 
           end_point = { x = expanded_half_size; y = expanded_half_size } };
    Line { start = { x = expanded_half_size; y = expanded_half_size }; 
           end_point = { x = -.expanded_half_size; y = expanded_half_size } };
    Line { start = { x = -.expanded_half_size; y = expanded_half_size }; 
           end_point = { x = -.expanded_half_size; y = -.expanded_half_size } };
    Line { start = { x = -.expanded_half_size; y = -.expanded_half_size }; 
           end_point = { x = expanded_half_size; y = -.expanded_half_size } };
  ]

let generate_rectangular_loop width height turn_number pitch is_inner =
  let op = if is_inner then (+.) else (-.) in
  let expanded_half_w = op (width *. 0.5) (pitch *. turn_number) in
  let expanded_half_h = op (height *. 0.5) (pitch *. turn_number) in
  [
    Line { start = { x = expanded_half_w; y = -.expanded_half_h }; 
           end_point = { x = expanded_half_w; y = expanded_half_h } };
    Line { start = { x = expanded_half_w; y = expanded_half_h }; 
           end_point = { x = -.expanded_half_w; y = expanded_half_h } };
    Line { start = { x = -.expanded_half_w; y = expanded_half_h }; 
           end_point = { x = -.expanded_half_w; y = -.expanded_half_h } };
    Line { start = { x = -.expanded_half_w; y = -.expanded_half_h }; 
           end_point = { x = expanded_half_w; y = -.expanded_half_h } };
  ]

let generate_oval_loop width height turn_number pitch is_inner =
  let op = if is_inner then (+.) else (-.) in
  let expanded_half_w = op (width *. 0.5) (pitch *. turn_number) in
  let expanded_half_h = op (height *. 0.5) (pitch *. turn_number) in
  let min_dim = min expanded_half_w expanded_half_h in
  let is_horizontal = width >= height in
  
  if is_horizontal then
    let straight_length = expanded_half_w -. min_dim in
    [
      Line { start = { x = -.straight_length; y = -.min_dim }; 
             end_point = { x = straight_length; y = -.min_dim } };
      Arc { start = { x = straight_length; y = -.min_dim }; 
            mid = { x = expanded_half_w; y = 0.0 }; 
            end_point = { x = straight_length; y = min_dim }; 
            radius = min_dim };
      Line { start = { x = straight_length; y = min_dim }; 
             end_point = { x = -.straight_length; y = min_dim } };
      Arc { start = { x = -.straight_length; y = min_dim }; 
            mid = { x = -.expanded_half_w; y = 0.0 }; 
            end_point = { x = -.straight_length; y = -.min_dim }; 
            radius = min_dim };
    ]
  else
    let straight_length = expanded_half_h -. min_dim in
    [
      Line { start = { x = -.min_dim; y = -.straight_length }; 
             end_point = { x = -.min_dim; y = straight_length } };
      Arc { start = { x = -.min_dim; y = straight_length }; 
            mid = { x = 0.0; y = expanded_half_h }; 
            end_point = { x = min_dim; y = straight_length }; 
            radius = min_dim };
      Line { start = { x = min_dim; y = straight_length }; 
             end_point = { x = min_dim; y = -.straight_length } };
      Arc { start = { x = min_dim; y = -.straight_length }; 
            mid = { x = 0.0; y = -.expanded_half_h }; 
            end_point = { x = -.min_dim; y = -.straight_length }; 
            radius = min_dim };
    ]

let generate_spiral_segments shape pitch turns is_inner =
  let loop_generator = match shape with
    | Round { diameter } -> generate_round_loop (diameter *. 0.5)
    | Square { size } -> generate_square_loop size
    | Rectangular { width; height } -> generate_rectangular_loop width height
    | Oval { width; height } -> generate_oval_loop width height
  in
  
  let num_turns = int_of_float (Float.ceil turns) in
  List.init num_turns (fun i -> 
    loop_generator (float_of_int i) pitch is_inner
  ) |> List.concat

let generate_spiral_path shape pitch turns is_inner =
  let all_segments = generate_spiral_segments shape pitch turns is_inner in
  
  (* Extract points from segments for compatibility with existing KiCad code *)
  let extract_points segments =
    List.fold_left (fun acc segment ->
      match segment with
      | Line { start; end_point } -> start :: end_point :: acc
      | Arc { start; end_point; _ } -> start :: end_point :: acc
    ) [] segments |> List.rev |> Array.of_list
  in
  
  extract_points all_segments