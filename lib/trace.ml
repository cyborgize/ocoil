let copper_resistivity_20c = 1.68e-8 (* Ohm·m at 20°C *)
let temperature_coefficient = 0.00393 (* per °C for copper *)

let oz_per_sqft_to_meters oz_per_sqft =
  let oz_to_kg = 0.0283495231 in
  let sqft_to_sqm = 0.09290304 in
  let copper_density = 8960.0 in
  let thickness_kg_per_sqm = (oz_per_sqft *. oz_to_kg) /. sqft_to_sqm in
  thickness_kg_per_sqm /. copper_density

let mil_to_meters mil =
  mil *. 25.4e-6

let calculate_resistivity_at_temp temp_celsius =
  copper_resistivity_20c *. (1.0 +. temperature_coefficient *. (temp_celsius -. 20.0))

let calculate_resistance length width thickness temperature =
  let area = width *. thickness in
  let resistivity = calculate_resistivity_at_temp temperature in
  let resistance = (resistivity *. length) /. area in
  resistance