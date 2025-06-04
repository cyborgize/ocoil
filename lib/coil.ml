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
      end_point : point;
    }
  | Arc of {
      start : point;
      mid : point;
      end_point : point;
      radius : float;
    }

type layer_segments = {
  layer_index : int;
  segments : path_segment list;
  last_point : point;
}

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

    Arc { start = start_point; mid = mid_point; end_point; radius = mid_radius })

let generate_square_loop ~size ~turn_number ~pitch ~is_inner ~is_last:_ ~trace_width:_ ~clearance:_ =
  let op = if is_inner then ( +. ) else ( -. ) in
  let current_half_size = op (size *. 0.5) (pitch *. turn_number) in
  let next_half_size = op (size *. 0.5) (pitch *. (turn_number +. 1.0)) in
  [
    Line
      {
        start = { x = current_half_size; y = op (-.current_half_size) pitch };
        end_point = { x = current_half_size; y = current_half_size };
      };
    Line
      {
        start = { x = current_half_size; y = current_half_size };
        end_point = { x = -.current_half_size; y = current_half_size };
      };
    Line
      {
        start = { x = -.current_half_size; y = current_half_size };
        end_point = { x = -.current_half_size; y = -.current_half_size };
      };
    Line
      {
        start = { x = -.current_half_size; y = -.current_half_size };
        end_point = { x = next_half_size; y = -.current_half_size };
      };
  ]

let generate_rectangular_loop ~width ~height ~turn_number ~pitch ~is_inner ~is_last:_ ~trace_width:_ ~clearance:_ =
  let op = if is_inner then ( +. ) else ( -. ) in
  let current_half_w = op (width *. 0.5) (pitch *. turn_number) in
  let current_half_h = op (height *. 0.5) (pitch *. turn_number) in
  let next_half_w = op (width *. 0.5) (pitch *. (turn_number +. 1.0)) in
  [
    Line
      {
        start = { x = current_half_w; y = op (-.current_half_h) pitch };
        end_point = { x = current_half_w; y = current_half_h };
      };
    Line
      { start = { x = current_half_w; y = current_half_h }; end_point = { x = -.current_half_w; y = current_half_h } };
    Line
      {
        start = { x = -.current_half_w; y = current_half_h };
        end_point = { x = -.current_half_w; y = -.current_half_h };
      };
    Line
      { start = { x = -.current_half_w; y = -.current_half_h }; end_point = { x = next_half_w; y = -.current_half_h } };
  ]

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
      | Line { start; end_point } ->
        Line { start = transform_point transform start; end_point = transform_point transform end_point }
      | Arc { start; mid; end_point; radius } ->
        Arc
          {
            start = transform_point transform start;
            mid = transform_point transform mid;
            end_point = transform_point transform end_point;
            radius;
          })
    segments

(* Calculate the stagger step size for via positioning *)
let calculate_via_stagger_step ~via_copper_size ~trace_width ~clearance =
  let effective_size = 0.5 *. (max trace_width via_copper_size +. via_copper_size) in
  effective_size +. clearance

(* Calculate via staggering offset along main axis *)
let calculate_via_offset ~layer_index ~via_copper_size ~trace_width ~clearance =
  let step = calculate_via_stagger_step ~via_copper_size ~trace_width ~clearance in
  float_of_int (layer_index / 2) *. step

(* Always assumes horizontal oval (main_dim >= across_dim) *)
let generate_oval_loop' ~main_dim ~across_dim ~turn_number ~pitch ~is_inner ~is_last ~trace_width ~clearance
  ~layer_index ~via_copper_size =
  let op = if is_inner then ( +. ) else ( -. ) in
  let current_half_main = op (main_dim *. 0.5) (pitch *. turn_number) in
  let current_half_across = op (across_dim *. 0.5) (pitch *. turn_number) in
  let next_half_main = op (main_dim *. 0.5) (pitch *. (turn_number +. 1.0)) in
  let next_half_across = op (across_dim *. 0.5) (pitch *. (turn_number +. 1.0)) in

  let straight_length = current_half_main -. current_half_across in
  let next_straight_length = next_half_main -. next_half_across in
  let arc_radius = current_half_across in
  (* Calculate via staggering offset along main axis *)
  let tail =
    match is_last with
    | true ->
      let arc_clearance = min 0. (arc_radius -. clearance -. trace_width) in
      let arc_radius' = arc_radius +. arc_clearance in
      let via_offset = calculate_via_offset ~layer_index ~via_copper_size ~trace_width ~clearance in
      let tail =
        match arc_clearance <= 0. with
        | true -> []
        | false ->
          [
            Line
              {
                start = { x = -.next_straight_length -. (0.5 *. pitch) +. via_offset; y = -.arc_clearance };
                end_point = { x = -.next_straight_length -. (0.5 *. pitch) +. via_offset; y = 0.0 };
              };
          ]
      in
      Line
        {
          start = { x = straight_length; y = arc_radius };
          end_point = { x = -.next_straight_length -. (0.5 *. pitch) +. arc_radius' +. via_offset; y = arc_radius };
        }
      :: Arc
           {
             start = { x = -.next_straight_length -. (0.5 *. pitch) +. arc_radius' +. via_offset; y = arc_radius };
             mid =
               {
                 x =
                   -.next_straight_length
                   -. (0.5 *. pitch)
                   +. arc_radius'
                   -. (arc_radius' *. 0.5 *. sqrt 2.0)
                   +. via_offset;
                 y = arc_radius -. arc_radius' +. (arc_radius' *. 0.5 *. sqrt 2.0);
               };
             end_point = { x = -.next_straight_length -. (0.5 *. pitch) +. via_offset; y = -.arc_clearance };
             radius = arc_radius';
           }
      :: tail
    | false ->
      [
        Line
          {
            start = { x = straight_length; y = arc_radius };
            end_point = { x = -.next_straight_length -. (0.5 *. pitch); y = arc_radius };
          };
        Arc
          {
            start = { x = -.next_straight_length -. (0.5 *. pitch); y = arc_radius };
            mid = { x = -.straight_length -. arc_radius; y = 0.5 *. pitch };
            end_point = { x = -.next_straight_length -. (0.5 *. pitch); y = -.next_half_across };
            radius = arc_radius;
          };
      ]
  in
  Line
    {
      start = { x = -.straight_length -. (0.5 *. pitch); y = -.arc_radius };
      end_point = { x = straight_length; y = -.arc_radius };
    }
  :: Arc
       {
         start = { x = straight_length; y = -.arc_radius };
         mid = { x = straight_length +. arc_radius; y = 0.0 };
         end_point = { x = straight_length; y = arc_radius };
         radius = arc_radius;
       }
  :: tail

let generate_oval_loop ~width ~height ~turn_number ~pitch ~is_inner ~is_last ~trace_width ~clearance ~layer_index
  ~via_copper_size =
  let main_dim, across_dim, transformation =
    if width >= height then
      (* Horizontal oval: no transformation needed *)
      width, height, identity
    else
      (* Vertical oval: swap dimensions and apply coordinate transformation *)
      height, width, swap_xy
  in
  let segments =
    generate_oval_loop' ~main_dim ~across_dim ~turn_number ~pitch ~is_inner ~is_last ~trace_width ~clearance
      ~layer_index ~via_copper_size
  in
  transform_segments transformation segments

let generate_spiral_segments_layer ~shape ~pitch ~turns ~is_inner ~trace_width ~clearance ~layer_index ~via_copper_size
    =
  let loop_generator turn_number is_last =
    match shape with
    | Round { diameter } ->
      generate_round_loop ~radius:(diameter *. 0.5) ~turn_number ~pitch ~is_inner ~is_last ~trace_width ~clearance
    | Square { size } -> generate_square_loop ~size ~turn_number ~pitch ~is_inner ~is_last ~trace_width ~clearance
    | Rectangular { width; height } ->
      generate_rectangular_loop ~width ~height ~turn_number ~pitch ~is_inner ~is_last ~trace_width ~clearance
    | Oval { width; height } ->
      generate_oval_loop ~width ~height ~turn_number ~pitch ~is_inner ~is_last ~trace_width ~clearance ~layer_index
        ~via_copper_size
  in

  let num_turns = int_of_float (Float.ceil turns) in
  let all_loops =
    List.init num_turns (fun i ->
      let is_last = i = pred num_turns in
      loop_generator (float_of_int i) is_last)
  in
  let segments = List.concat all_loops in

  let transformation =
    if layer_index mod 2 = 1 then (
      (* Mirror segments across the shorter dimension for odd layers *)
      match shape with
      | Round _ | Square _ -> mirror_x_axis
      | Rectangular { width; height } | Oval { width; height } ->
        if width < height then mirror_y_axis else mirror_x_axis)
    else identity
  in
  transform_segments transformation segments

(* Calculate the last point coordinates for a coil layer *)
let calculate_last_point ~shape ~pitch ~turns ~layer_index ~via_copper_size ~trace_width ~clearance =
  match shape with
  | Round { diameter } ->
    (* For round coils, the last point is at the inner radius *)
    let radius = (diameter *. 0.5) -. (pitch *. turns) in
    { x = radius; y = 0.0 }
  | Square { size } ->
    (* For square coils, the last point is at the inner edge *)
    let half_size = (size *. 0.5) -. (pitch *. turns) in
    { x = half_size; y = -.half_size }
  | Rectangular { width; height } ->
    (* For rectangular coils, the last point is at the inner edge *)
    let half_width = (width *. 0.5) -. (pitch *. turns) in
    let half_height = (height *. 0.5) -. (pitch *. turns) in
    { x = half_width; y = -.half_height }
  | Oval { width; height } ->
    (* For oval coils, calculate based on main and across dimensions *)
    let main_dim = max width height in
    let across_dim = min width height in
    (* Calculate via staggering offset along main axis *)
    let via_offset = calculate_via_offset ~layer_index ~via_copper_size ~trace_width ~clearance in
    let last_coord = (-0.5 *. (pitch +. (main_dim -. across_dim))) +. via_offset in
    if width >= height then
      (* Horizontal oval: main axis is X, across axis is Y *)
      { x = last_coord; y = 0.0 }
    else
      (* Vertical oval: main axis is Y, across axis is X *)
      { x = 0.0; y = last_coord }

let generate_spiral_segments ~shape ~pitch ~turns ~is_inner ~trace_width ~clearance ~layers ~via_copper_size =
  List.init layers (fun layer_index ->
    let segments =
      generate_spiral_segments_layer ~shape ~pitch ~turns ~is_inner ~trace_width ~clearance ~layer_index
        ~via_copper_size
    in
    let last_point = calculate_last_point ~shape ~pitch ~turns ~layer_index ~via_copper_size ~trace_width ~clearance in
    { layer_index; segments; last_point })
