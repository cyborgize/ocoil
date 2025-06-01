type coil_shape = 
  | Round of { diameter : float }
  | Square of { size : float }
  | Rectangular of { width : float; height : float }
  | Oval of { width : float; height : float }

type point = { x: float; y: float }

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