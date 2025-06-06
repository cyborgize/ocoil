type coil_shape =
  | Round of { diameter : float }
  | Square of { size : float }
  | Rectangular of {
      width : float;
      height : float;
    }
  | Oval of {
      width : float;
      height : float;
    }

type point = {
  x : float;
  y : float;
}

type path_segment =
  | Line of {
      start : point;
      end_ : point;
    }
  | Arc of {
      start : point;
      mid : point;
      end_ : point;
      radius : float;
    }

type layer_segments = {
  layer_index : int;
  segments : path_segment list;
  first_point : point option;
  last_point : point option;
}

type 'a loop_result = {
  segments : 'a list;
  first_point : point option;
  last_point : point option;
}

(* Helper functions to create path segments *)
let make_line ~start ~end_ = Line { start; end_ }

let make_arc ~start ~mid ~end_ ~radius = Arc { start; mid; end_; radius }

let calculate_spiral_length ~shape ~pitch ~turns ~is_inner =
  let op = if is_inner then ( +. ) else ( -. ) in
  let radial_expansion = 2.0 *. pitch *. turns in

  let avg_circumference =
    match shape with
    | Round { diameter } ->
      let other_diameter = op diameter radial_expansion in
      let avg_diameter = (diameter +. other_diameter) *. 0.5 in
      Float.pi *. avg_diameter
    | Square { size } ->
      let other_size = op size radial_expansion in
      let avg_size = (size +. other_size) *. 0.5 in
      4.0 *. avg_size
    | Rectangular { width; height } ->
      let avg_w = (width +. op width radial_expansion) *. 0.5 in
      let avg_h = (height +. op height radial_expansion) *. 0.5 in
      2.0 *. (avg_w +. avg_h)
    | Oval { width; height } ->
      let avg_w = (width +. op width radial_expansion) *. 0.5 in
      let avg_h = (height +. op height radial_expansion) *. 0.5 in
      let min_dim = min avg_w avg_h in
      let max_dim = max avg_w avg_h in
      let straight_length = max_dim -. min_dim in
      (* Slot-shaped oval: two straight segments + semicircular ends *)
      let semicircular_length = Float.pi *. min_dim in
      (2.0 *. straight_length) +. semicircular_length
  in
  avg_circumference *. turns

let generate_round_loop ~radius ~turn_number ~pitch ~is_inner ~is_last:_ ~trace_width:_ ~clearance:_ =
  let op = if is_inner then ( +. ) else ( -. ) in
  let angle_offset = Float.pi *. 2.0 *. turn_number in
  let current_radius = op radius (pitch *. turn_number) in
  let next_radius = op radius (pitch *. (turn_number +. 1.0)) in
  let num_arcs = 8 in
  let angle_step = Float.pi *. 2.0 /. float_of_int num_arcs in

  let segments =
    List.init num_arcs (fun i ->
      let start_angle = angle_offset +. (float_of_int i *. angle_step) in
      let end_angle = angle_offset +. (float_of_int (i + 1) *. angle_step) in
      let mid_angle = (start_angle +. end_angle) *. 0.5 in

      (* Calculate the radius progression for this arc *)
      let arc_progress = float_of_int i /. float_of_int num_arcs in
      let next_arc_progress = float_of_int (i + 1) /. float_of_int num_arcs in
      let mid_progress = (arc_progress +. next_arc_progress) *. 0.5 in

      let start_radius = current_radius +. ((next_radius -. current_radius) *. arc_progress) in
      let end_radius = current_radius +. ((next_radius -. current_radius) *. next_arc_progress) in
      let mid_radius = current_radius +. ((next_radius -. current_radius) *. mid_progress) in

      let start_point = { x = start_radius *. cos start_angle; y = start_radius *. sin start_angle } in
      let mid_point = { x = mid_radius *. cos mid_angle; y = mid_radius *. sin mid_angle } in
      let end_point = { x = end_radius *. cos end_angle; y = end_radius *. sin end_angle } in

      make_arc ~start:start_point ~mid:mid_point ~end_:end_point ~radius:mid_radius)
  in

  let first_point = Some { x = current_radius *. cos angle_offset; y = current_radius *. sin angle_offset } in
  let last_point =
    Some
      {
        x = next_radius *. cos (angle_offset +. (Float.pi *. 2.0));
        y = next_radius *. sin (angle_offset +. (Float.pi *. 2.0));
      }
  in

  { segments; first_point; last_point }

let generate_rectangular_loop ~width ~height ~turn_number ~pitch ~is_inner ~is_last:_ ~trace_width:_ ~clearance:_ =
  let op = if is_inner then ( +. ) else ( -. ) in
  let current_half_w = op (width *. 0.5) (pitch *. turn_number) in
  let current_half_h = op (height *. 0.5) (pitch *. turn_number) in
  let next_half_w = op (width *. 0.5) (pitch *. (turn_number +. 1.0)) in

  (* Compute all points first *)
  let p1 = { x = current_half_w; y = op (-.current_half_h) pitch } in
  let p2 = { x = current_half_w; y = current_half_h } in
  let p3 = { x = -.current_half_w; y = current_half_h } in
  let p4 = { x = -.current_half_w; y = -.current_half_h } in
  let p5 = { x = next_half_w; y = -.current_half_h } in

  let segments =
    [
      make_line ~start:p1 ~end_:p2;
      make_line ~start:p2 ~end_:p3;
      make_line ~start:p3 ~end_:p4;
      make_line ~start:p4 ~end_:p5;
    ]
  in

  let first_point = Some p1 in
  let last_point = Some p5 in

  { segments; first_point; last_point }

(* Transformation matrix type: 2x2 matrix as (a, b, c, d) where:
   [a b] [x]   [a*x + b*y]
   [c d] [y] = [c*x + d*y]
*)
type transform_matrix = float * float * float * float

(* Predefined transformation matrices *)
let identity = 1.0, 0.0, 0.0, 1.0 (* Identity matrix (no transformation) *)
let mirror_x_axis = 1.0, 0.0, 0.0, -1.0 (* Mirror across X axis (flip Y coordinates) *)
let mirror_y_axis = -1.0, 0.0, 0.0, 1.0 (* Mirror across Y axis (flip X coordinates) *)
let swap_xy = 0.0, 1.0, 1.0, 0.0 (* Swap X and Y coordinates *)

(* Apply transformation to a point using matrix multiplication *)
let transform_point (a, b, c, d) point = { x = (a *. point.x) +. (b *. point.y); y = (c *. point.x) +. (d *. point.y) }

(* Apply transformation to a list of path segments *)
let transform_segments transform segments =
  List.map
    (function
      | Line { start; end_ } -> Line { start = transform_point transform start; end_ = transform_point transform end_ }
      | Arc { start; mid; end_; radius } ->
        Arc
          {
            start = transform_point transform start;
            mid = transform_point transform mid;
            end_ = transform_point transform end_;
            radius;
          })
    segments

(* Calculate minimum distance between via and trace  *)
let calculate_via_trace_distance ~via_copper_size ~trace_width ~clearance =
  let effective_size = 0.5 *. (max trace_width via_copper_size +. trace_width) in
  effective_size +. clearance

(* Calculate the stagger step size for via positioning *)
let calculate_via_stagger_step ~via_copper_size ~trace_width ~clearance =
  let effective_size = 0.5 *. (max trace_width via_copper_size +. via_copper_size) in
  effective_size +. clearance

(* Calculate via staggering offset along main axis *)
let calculate_via_offset ~layer_index ~via_copper_size ~trace_width ~clearance =
  let step = calculate_via_stagger_step ~via_copper_size ~trace_width ~clearance in
  float_of_int (layer_index / 2) *. step

(* Calculate desired Y coordinate for prepended arc main axis point *)
let calculate_prepend_y_coordinate ~total_layers ~layer_index ~via_copper_size ~trace_width ~clearance =
  if total_layers <= 4 then
    (* Up to 4 layers: single point at Y=0 *)
    0.0
  else (
    (* 5-6 layers: two points staggered symmetrically around main axis *)
    let stagger_step = calculate_via_stagger_step ~via_copper_size ~trace_width ~clearance in
    let half_step = stagger_step *. 0.5 in
    if (layer_index + (layer_index / 3)) mod 2 = 1 then -.half_step else half_step)

(* Helper function to conditionally prepend to a list *)
let cons opt_x l =
  match opt_x with
  | Some x -> x :: l
  | None -> l

(* Always assumes horizontal oval (main_dim >= across_dim) *)
let generate_oval_loop' ~main_dim ~across_dim ~turn_number ~pitch ~is_inner ~is_last ~trace_width ~clearance
  ~layer_index ~via_copper_size ~total_layers =
  let op = if is_inner then ( +. ) else ( -. ) in
  let current_half_main = op (main_dim *. 0.5) (pitch *. turn_number) in
  let current_half_across = op (across_dim *. 0.5) (pitch *. turn_number) in
  let next_half_main = op (main_dim *. 0.5) (pitch *. (turn_number +. 1.0)) in
  let next_half_across = op (across_dim *. 0.5) (pitch *. (turn_number +. 1.0)) in

  let straight_length = current_half_main -. current_half_across in
  let next_straight_length = next_half_main -. next_half_across in
  let arc_radius = current_half_across in

  (* Check if this is the first loop on first layer or last odd layer *)
  let is_first_loop = turn_number = 0.0 in
  let is_last_layer = layer_index = total_layers - 1 in
  let is_last_layer_odd = is_last_layer && layer_index mod 2 = 1 in
  let use_main_coordinate = is_first_loop && (layer_index = 0 || is_last_layer_odd) in

  (* Check if we need to prepend arc and tangent line for first loop on all layers except first and last *)
  let is_first_layer = layer_index = 0 in
  let prepend_arc_line = is_first_loop && (not is_first_layer) && not is_last_layer in

  (* Calculate via staggering offset along main axis *)
  let tail =
    match is_last with
    | true ->
      let arc_clearance = min 0. (arc_radius -. clearance -. trace_width) in
      let arc_radius' = arc_radius +. arc_clearance in
      let via_offset = calculate_via_offset ~layer_index ~via_copper_size ~trace_width ~clearance in

      (* Compute points for tail segments *)
      let line_start_x = -.next_straight_length -. (0.5 *. pitch) +. via_offset in
      let optional_line_start = { x = line_start_x; y = -.arc_clearance } in
      let optional_line_end = { x = line_start_x; y = 0.0 } in
      let line1_start = { x = straight_length; y = arc_radius } in
      let line1_end = { x = -.next_straight_length -. (0.5 *. pitch) +. arc_radius' +. via_offset; y = arc_radius } in
      let arc_start = line1_end in
      let arc_mid =
        {
          x = -.next_straight_length -. (0.5 *. pitch) +. arc_radius' -. (arc_radius' *. 0.5 *. sqrt 2.0) +. via_offset;
          y = arc_radius -. arc_radius' +. (arc_radius' *. 0.5 *. sqrt 2.0);
        }
      in
      let arc_end = { x = line_start_x; y = -.arc_clearance } in

      let optional_line =
        if arc_clearance <= 0. then None else Some (make_line ~start:optional_line_start ~end_:optional_line_end)
      in

      make_line ~start:line1_start ~end_:line1_end
      :: make_arc ~start:arc_start ~mid:arc_mid ~end_:arc_end ~radius:arc_radius'
      :: cons optional_line []
    | false ->
      (* Compute points for false case segments *)
      let line_start = { x = straight_length; y = arc_radius } in
      let line_end = { x = -.next_straight_length -. (0.5 *. pitch); y = arc_radius } in
      let arc_start = line_end in
      let arc_mid = { x = -.straight_length -. arc_radius; y = 0.5 *. pitch } in
      let arc_end = { x = -.next_straight_length -. (0.5 *. pitch); y = -.next_half_across } in

      [
        make_line ~start:line_start ~end_:line_end;
        make_arc ~start:arc_start ~mid:arc_mid ~end_:arc_end ~radius:arc_radius;
      ]
  in
  let main_segments =
    (* Compute points for main segments *)
    let main_line_start =
      {
        x = (if use_main_coordinate then -.current_half_main else -.straight_length -. (0.5 *. pitch));
        y = -.arc_radius;
      }
    in
    let main_line_end = { x = straight_length; y = -.arc_radius } in
    let main_arc_start = main_line_end in
    let main_arc_mid = { x = straight_length +. arc_radius; y = 0.0 } in
    let main_arc_end = { x = straight_length; y = arc_radius } in

    make_line ~start:main_line_start ~end_:main_line_end
    :: make_arc ~start:main_arc_start ~mid:main_arc_mid ~end_:main_arc_end ~radius:arc_radius
    :: tail
  in

  (* Prepend arc and tangent line for odd layers (except last) *)
  if prepend_arc_line then (
    let arc_center_x = -.straight_length -. (0.5 *. pitch) in
    let arc_center_y = 0.5 *. pitch in
    let prepend_arc_radius = arc_radius +. (0.5 *. pitch) in
    let next_arc_radius = next_half_across in
    let via_offset = calculate_via_trace_distance ~via_copper_size ~trace_width ~clearance in
    (* Calculate the main axis point coordinates considering desired Y coordinate *)
    let desired_distance = next_arc_radius +. (0.5 *. pitch) +. via_offset in
    let desired_y =
      calculate_prepend_y_coordinate ~total_layers ~layer_index ~via_copper_size ~trace_width ~clearance
    in
    let dy_to_center = desired_y -. arc_center_y in
    (* Y offset from point to arc center *)
    let dx_to_center = sqrt ((desired_distance *. desired_distance) -. (dy_to_center *. dy_to_center)) in
    let main_axis_point = { x = arc_center_x -. dx_to_center; y = desired_y } in

    (* Calculate tangent point on the arc *)
    let dx = main_axis_point.x -. arc_center_x in
    let dy = main_axis_point.y -. arc_center_y in
    let distance = sqrt ((dx *. dx) +. (dy *. dy)) in

    let tangent_point =
      if distance <= prepend_arc_radius then
        (* Point is inside or on the circle, fall back to perpendicular point *)
        {
          x = arc_center_x +. (dx *. prepend_arc_radius /. distance);
          y = arc_center_y +. (dy *. prepend_arc_radius /. distance);
        }
      else (
        (* Point is outside the circle, calculate true tangent *)
        let tangent_length = sqrt ((distance *. distance) -. (prepend_arc_radius *. prepend_arc_radius)) in
        let sin_angle = tangent_length /. distance in
        let cos_angle = prepend_arc_radius /. distance in
        {
          x = arc_center_x +. ((dx *. cos_angle) -. (dy *. sin_angle));
          y = arc_center_y +. ((dy *. cos_angle) +. (dx *. sin_angle));
        })
    in

    (* Compute points for prepend segments *)
    let prepend_line_start = main_axis_point in
    let prepend_line_end = tangent_point in
    let prepend_arc_start = tangent_point in
    let prepend_arc_mid =
      {
        x = -.straight_length -. (0.5 *. pitch) -. (0.5 *. sqrt 2.0 *. prepend_arc_radius);
        y = (0.5 *. pitch) -. (0.5 *. sqrt 2.0 *. prepend_arc_radius);
      }
    in
    let prepend_arc_end = { x = -.straight_length -. (0.5 *. pitch); y = -.arc_radius } in

    let all_segments =
      make_line ~start:prepend_line_start ~end_:prepend_line_end
      :: make_arc ~start:prepend_arc_start ~mid:prepend_arc_mid ~end_:prepend_arc_end ~radius:prepend_arc_radius
      :: main_segments
    in
    let first_point = Some main_axis_point in
    let last_point =
      if is_last then (
        let via_offset = calculate_via_offset ~layer_index ~via_copper_size ~trace_width ~clearance in
        Some { x = -.next_straight_length -. (0.5 *. pitch) +. via_offset; y = 0.0 })
      else Some { x = -.next_straight_length -. (0.5 *. pitch); y = -.next_half_across }
    in
    { segments = all_segments; first_point; last_point })
  else (
    let first_point =
      Some
        {
          x = (if use_main_coordinate then -.current_half_main else -.straight_length -. (0.5 *. pitch));
          y = -.arc_radius;
        }
    in
    let last_point =
      if is_last then (
        let via_offset = calculate_via_offset ~layer_index ~via_copper_size ~trace_width ~clearance in
        Some { x = -.next_straight_length -. (0.5 *. pitch) +. via_offset; y = 0.0 })
      else Some { x = -.next_straight_length -. (0.5 *. pitch); y = -.next_half_across }
    in
    { segments = main_segments; first_point; last_point })

let generate_oval_loop ~width ~height ~turn_number ~pitch ~is_inner ~is_last ~trace_width ~clearance ~layer_index
  ~via_copper_size ~total_layers =
  let main_dim, across_dim, transformation =
    if width >= height then
      (* Horizontal oval: no transformation needed *)
      width, height, identity
    else
      (* Vertical oval: swap dimensions and apply coordinate transformation *)
      height, width, swap_xy
  in
  let loop_result =
    generate_oval_loop' ~main_dim ~across_dim ~turn_number ~pitch ~is_inner ~is_last ~trace_width ~clearance
      ~layer_index ~via_copper_size ~total_layers
  in
  let transformed_segments = transform_segments transformation loop_result.segments in
  let transformed_first_point = Option.map (transform_point transformation) loop_result.first_point in
  let transformed_last_point = Option.map (transform_point transformation) loop_result.last_point in
  { segments = transformed_segments; first_point = transformed_first_point; last_point = transformed_last_point }

let generate_spiral_segments_layer ~shape ~pitch ~turns ~is_inner ~trace_width ~clearance ~layer_index ~via_copper_size
  ~total_layers =
  let loop_generator turn_number is_last =
    match shape with
    | Round { diameter } ->
      generate_round_loop ~radius:(diameter *. 0.5) ~turn_number ~pitch ~is_inner ~is_last ~trace_width ~clearance
    | Square { size } ->
      generate_rectangular_loop ~width:size ~height:size ~turn_number ~pitch ~is_inner ~is_last ~trace_width ~clearance
    | Rectangular { width; height } ->
      generate_rectangular_loop ~width ~height ~turn_number ~pitch ~is_inner ~is_last ~trace_width ~clearance
    | Oval { width; height } ->
      generate_oval_loop ~width ~height ~turn_number ~pitch ~is_inner ~is_last ~trace_width ~clearance ~layer_index
        ~via_copper_size ~total_layers
  in

  let num_turns = int_of_float (Float.ceil turns) in

  (* Fold over loops to accumulate segments and track first/last points *)
  let accumulator = { segments = []; first_point = None; last_point = None } in
  let { segments; first_point; last_point } =
    List.fold_left
      (fun acc i ->
        let is_last = i = pred num_turns in
        let loop_result = loop_generator (float_of_int i) is_last in
        {
          segments = loop_result.segments :: acc.segments;
          first_point = Option.fold ~none:loop_result.first_point ~some:Option.some acc.first_point;
          last_point = Option.fold ~none:acc.last_point ~some:Option.some loop_result.last_point;
        })
      accumulator (List.init num_turns Fun.id)
  in
  let segments = List.concat segments in

  let transformation =
    if layer_index mod 2 = 1 then (
      (* Mirror segments across the shorter dimension for odd layers *)
      match shape with
      | Round _ | Square _ -> mirror_x_axis
      | Rectangular { width; height } | Oval { width; height } ->
        if width < height then mirror_y_axis else mirror_x_axis)
    else identity
  in

  let first_point, last_point = if layer_index mod 2 = 0 then first_point, last_point else last_point, first_point in
  {
    layer_index;
    segments = transform_segments transformation segments;
    first_point = Option.map (transform_point transformation) first_point;
    last_point = Option.map (transform_point transformation) last_point;
  }

let generate_spiral_segments ~shape ~pitch ~turns ~is_inner ~trace_width ~clearance ~layers ~via_copper_size =
  List.init layers (fun layer_index ->
    generate_spiral_segments_layer ~shape ~pitch ~turns ~is_inner ~trace_width ~clearance ~layer_index ~via_copper_size
      ~total_layers:layers)
