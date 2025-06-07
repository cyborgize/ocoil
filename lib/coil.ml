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
      corner_radius : float option;
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

let calculate_spiral_length ~shape ~pitch ~turns =
  let radial_expansion = 2.0 *. pitch *. turns in

  let avg_circumference =
    match shape with
    | Round { diameter } ->
      let inner_diameter = diameter -. radial_expansion in
      let avg_diameter = (diameter +. inner_diameter) *. 0.5 in
      Float.pi *. avg_diameter
    | Square { size } ->
      let inner_size = size -. radial_expansion in
      let avg_size = (size +. inner_size) *. 0.5 in
      4.0 *. avg_size
    | Rectangular { width; height } ->
      let inner_width = width -. radial_expansion in
      let inner_height = height -. radial_expansion in
      let avg_w = (width +. inner_width) *. 0.5 in
      let avg_h = (height +. inner_height) *. 0.5 in
      2.0 *. (avg_w +. avg_h)
    | Oval { width; height; corner_radius = _ } ->
      let inner_width = width -. radial_expansion in
      let inner_height = height -. radial_expansion in
      let avg_w = (width +. inner_width) *. 0.5 in
      let avg_h = (height +. inner_height) *. 0.5 in
      let min_dim = min avg_w avg_h in
      let max_dim = max avg_w avg_h in
      let straight_length = max_dim -. min_dim in
      (* Slot-shaped oval: two straight segments + semicircular ends *)
      let semicircular_length = Float.pi *. min_dim in
      (2.0 *. straight_length) +. semicircular_length
  in
  avg_circumference *. turns

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

(* Helper function to check if a line segment is nearly zero length *)
let is_line_nearly_zero ~start ~end_ =
  let dx = end_.x -. start.x in
  let dy = end_.y -. start.y in
  let length = sqrt ((dx *. dx) +. (dy *. dy)) in
  length < 1e-6

(* Helper function to check if a scalar length is nearly zero *)
let is_length_nearly_zero length = abs_float length < 1e-6

(* Helper function to check if a path segment is degenerate *)
let is_segment_degenerate = function
  | Line { start; end_ } -> is_line_nearly_zero ~start ~end_
  | Arc { start; end_; radius; _ } ->
    (* Arc is degenerate if start and end are the same or radius is nearly zero *)
    is_line_nearly_zero ~start ~end_ || is_length_nearly_zero radius

(* Helper function to map out degenerate segments *)
let map_good_segment segment = if is_segment_degenerate segment then None else Some segment

(* Always assumes horizontal oval (main_dim >= across_dim) *)
let generate_oval_loop' ?corner_radius ~main_dim ~across_dim ~turn_number ~pitch ~is_last ~trace_width ~clearance
  ~layer_index ~via_copper_size ~total_layers () =
  let current_half_main = (main_dim *. 0.5) -. (pitch *. turn_number) in
  let current_half_across = (across_dim *. 0.5) -. (pitch *. turn_number) in
  let _next_half_main = (main_dim *. 0.5) -. (pitch *. (turn_number +. 1.0)) in
  let next_half_across = (across_dim *. 0.5) -. (pitch *. (turn_number +. 1.0)) in
  let arc_radius =
    match corner_radius with
    | Some corner_radius -> corner_radius -. (pitch *. turn_number)
    | None -> current_half_across
  in
  let arc_radius = max 0. arc_radius in
  let next_arc_radius = max 0. (arc_radius -. pitch) in
  let mid_arc_radius = 0.5 *. (arc_radius +. next_arc_radius) in
  let straight_length = current_half_main -. arc_radius in
  let mid_straight_length = current_half_main -. mid_arc_radius in
  let next_straight_length = straight_length -. pitch in
  (* Check if this is the first loop on first layer or last odd layer *)
  let is_first_loop = turn_number = 0.0 in
  let is_last_layer = layer_index = total_layers - 1 in
  let is_last_layer_odd = is_last_layer && layer_index mod 2 = 1 in
  let use_main_coordinate = is_first_loop && (layer_index = 0 || is_last_layer_odd) in
  (* Check if we need to prepend arc and tangent line for first loop on all layers except first and last *)
  let is_first_layer = layer_index = 0 in
  let prepend_arc_line = is_first_loop && (not is_first_layer) && not is_last_layer in
  (* Calculate via staggering offset along main axis *)
  let tail, last_point =
    match is_last with
    | true ->
      let arc_clearance = min 0. (current_half_across -. clearance -. trace_width) in
      let arc_radius' = max 0. (arc_radius +. arc_clearance) in
      let via_offset = calculate_via_offset ~layer_index ~via_copper_size ~trace_width ~clearance in
      let line_start_x = -.mid_straight_length +. via_offset in
      let arc_center_x = line_start_x +. arc_radius' in
      (* Compute points for tail segments *)
      let optional_line_start = { x = line_start_x; y = current_half_across -. arc_radius' } in
      let optional_line_end = { x = line_start_x; y = -.arc_clearance } in
      let line1_start = { x = straight_length; y = current_half_across } in
      let line1_end = { x = arc_center_x; y = current_half_across } in
      let arc_start = line1_end in
      let arc_mid =
        {
          x = arc_center_x -. (arc_radius' *. 0.5 *. sqrt 2.0);
          y = current_half_across -. arc_radius' +. (arc_radius' *. 0.5 *. sqrt 2.0);
        }
      in
      let arc_end = { x = line_start_x; y = current_half_across -. arc_radius' } in
      let segments =
        [
          make_line ~start:line1_start ~end_:line1_end;
          make_arc ~start:arc_start ~mid:arc_mid ~end_:arc_end ~radius:arc_radius';
          make_line ~start:optional_line_start ~end_:optional_line_end;
        ]
      in
      let last_point =
        let via_offset = calculate_via_offset ~layer_index ~via_copper_size ~trace_width ~clearance in
        Some { x = -.mid_straight_length +. via_offset; y = 0.0 }
      in
      segments, last_point
    | false ->
      let line_start = { x = straight_length; y = current_half_across } in
      let line_end = { x = -.mid_straight_length; y = current_half_across } in
      (* Handle tail corner arcs based on next_arc_radius *)
      let tail_corner_segments =
        (* Non-zero corner radius: split into two half-arcs with optional connecting line *)
        let tail_arc_start = line_end in
        (* The arc center is at the corner, offset by 0.5 * pitch *)
        let arc_center_x = -.mid_straight_length in
        let first_arc_center_y = current_half_across -. mid_arc_radius in
        let second_arc_center_y = -.next_half_across +. mid_arc_radius in
        (* First half-arc: from top (arc_radius) to middle (0.5 * pitch) *)
        let first_arc_start = tail_arc_start in
        let first_arc_end = { x = arc_center_x -. mid_arc_radius; y = first_arc_center_y } in
        let first_arc_mid =
          {
            x = arc_center_x -. (mid_arc_radius *. sqrt 2.0 /. 2.0);
            y = first_arc_center_y +. (mid_arc_radius *. sqrt 2.0 /. 2.0);
          }
        in
        (* Second half-arc: from middle (0.5 * pitch) to bottom (-.next_half_across) *)
        let second_arc_start = { x = arc_center_x -. mid_arc_radius; y = second_arc_center_y } in
        let second_arc_end = { x = -.mid_straight_length; y = -.next_half_across } in
        let second_arc_mid =
          {
            x = arc_center_x -. (mid_arc_radius *. sqrt 2.0 /. 2.0);
            y = second_arc_center_y -. (mid_arc_radius *. sqrt 2.0 /. 2.0);
          }
        in
        [
          make_arc ~start:first_arc_start ~mid:first_arc_mid ~end_:first_arc_end ~radius:mid_arc_radius;
          make_line ~start:first_arc_end ~end_:second_arc_start;
          make_arc ~start:second_arc_start ~mid:second_arc_mid ~end_:second_arc_end ~radius:mid_arc_radius;
        ]
      in
      let segments = make_line ~start:line_start ~end_:line_end :: tail_corner_segments in
      let last_point = Some { x = -.next_straight_length; y = -.next_half_across } in
      segments, last_point
  in
  let segments =
    (* Compute points for main segments *)
    let main_line_start =
      {
        x =
          (if use_main_coordinate then -.current_half_main
           else -.straight_length -. pitch +. (arc_radius -. mid_arc_radius));
        y = -.current_half_across;
      }
    in
    let main_line_end = { x = straight_length; y = -.current_half_across } in
    (* Handle corner arcs based on corner radius *)
    let segments =
      (* Non-zero corner radius: split into two half-arcs with optional connecting line *)
      let main_arc_start = main_line_end in
      (* The arc center is at the corner of the original oval *)
      let arc_center_x = straight_length in
      let arc_center_y = current_half_across -. arc_radius in
      (* First half-arc: from bottom (-current_half_across) to middle (0) *)
      let first_arc_start = main_arc_start in
      let first_arc_end = { x = arc_center_x +. arc_radius; y = -.arc_center_y } in
      let first_arc_mid =
        { x = arc_center_x +. (arc_radius *. sqrt 2.0 /. 2.0); y = -.arc_center_y -. (arc_radius *. sqrt 2.0 /. 2.0) }
      in
      (* Second half-arc: from middle (0) to top (+current_half_across) *)
      let second_arc_start = { x = arc_center_x +. arc_radius; y = arc_center_y } in
      let second_arc_end = { x = straight_length; y = current_half_across } in
      let second_arc_mid =
        { x = arc_center_x +. (arc_radius *. sqrt 2.0 /. 2.0); y = arc_center_y +. (arc_radius *. sqrt 2.0 /. 2.0) }
      in
      make_arc ~start:first_arc_start ~mid:first_arc_mid ~end_:first_arc_end ~radius:arc_radius
      :: make_line ~start:first_arc_end ~end_:second_arc_start
      :: make_arc ~start:second_arc_start ~mid:second_arc_mid ~end_:second_arc_end ~radius:arc_radius
      :: tail
    in
    make_line ~start:main_line_start ~end_:main_line_end :: segments
  in
  let segments, first_point =
    (* Maybe prepend arc and tangent line for odd layers (except last) *)
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
        :: segments
      in
      let first_point = Some main_axis_point in
      all_segments, first_point)
    else (
      let first_point =
        Some
          {
            x = (if use_main_coordinate then -.current_half_main else -.straight_length -. (0.5 *. pitch));
            y = -.current_half_across;
          }
      in
      segments, first_point)
  in
  { segments = List.filter_map map_good_segment segments; first_point; last_point }

let generate_oval_loop ?corner_radius ~width ~height ~turn_number ~pitch ~is_last ~trace_width ~clearance ~layer_index
  ~via_copper_size ~total_layers () =
  let main_dim, across_dim, transformation =
    if width >= height then
      (* Horizontal oval: no transformation needed *)
      width, height, identity
    else
      (* Vertical oval: swap dimensions and apply coordinate transformation *)
      height, width, swap_xy
  in
  let loop_result =
    generate_oval_loop' ?corner_radius ~main_dim ~across_dim ~turn_number ~pitch ~is_last ~trace_width ~clearance
      ~layer_index ~via_copper_size ~total_layers ()
  in
  let transformed_segments = transform_segments transformation loop_result.segments in
  let transformed_first_point = Option.map (transform_point transformation) loop_result.first_point in
  let transformed_last_point = Option.map (transform_point transformation) loop_result.last_point in
  { segments = transformed_segments; first_point = transformed_first_point; last_point = transformed_last_point }

let generate_spiral_segments_layer ~shape ~pitch ~turns ~trace_width ~clearance ~layer_index ~via_copper_size
  ~total_layers =
  let loop_generator turn_number is_last =
    match shape with
    | Round { diameter } ->
      generate_oval_loop ~width:diameter ~height:diameter ~turn_number ~pitch ~is_last ~trace_width ~clearance
        ~layer_index ~via_copper_size ~total_layers ()
    | Square { size } ->
      generate_oval_loop ~corner_radius:0.0 ~width:size ~height:size ~turn_number ~pitch ~is_last ~trace_width
        ~clearance ~layer_index ~via_copper_size ~total_layers ()
    | Rectangular { width; height } ->
      generate_oval_loop ~corner_radius:0.0 ~width ~height ~turn_number ~pitch ~is_last ~trace_width ~clearance
        ~layer_index ~via_copper_size ~total_layers ()
    | Oval { width; height; corner_radius } ->
      generate_oval_loop ?corner_radius ~width ~height ~turn_number ~pitch ~is_last ~trace_width ~clearance ~layer_index
        ~via_copper_size ~total_layers ()
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
      | Rectangular { width; height } | Oval { width; height; corner_radius = _ } ->
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

let generate_spiral_segments ~shape ~pitch ~turns ~trace_width ~clearance ~layers ~via_copper_size =
  List.init layers (fun layer_index ->
    generate_spiral_segments_layer ~shape ~pitch ~turns ~trace_width ~clearance ~layer_index ~via_copper_size
      ~total_layers:layers)
