type vec3 = float * float * float
type dir3 = X | Y | Z

let unitx : vec3 = (1.0, 0.0, 0.0)
let unity : vec3 = (0.0, 1.0, 0.0)
let unitz : vec3 = (0.0, 0.0, 1.0)
let zero3 : vec3 = (0.0, 0.0, 0.0)

let vec3_eq ?(tol = 1e-7) ((x1, y1, z1) : vec3) ((x2, y2, z2) : vec3) =
  Float.abs (x1 -. x2) < tol
  && Float.abs (y1 -. y2) < tol
  && Float.abs (z1 -. z2) < tol

let vec3_map fn ((x, y, z) : vec3) = (fn x, fn y, fn z)

let vec3_map2 fn ((x1, y1, z1) : vec3) ((x2, y2, z2) : vec3) =
  (fn x1 x2, fn y1 y2, fn z1 z2)

let addv = vec3_map2 ( +. )
let subv = vec3_map2 ( -. )
let scale k = vec3_map (( *. ) k)
let scale_inv k = vec3_map (fun x -> x /. k)

let dot v1 v2 =
  let x, y, z = vec3_map2 ( *. ) v1 v2 in
  x +. y +. z

let len v = dot v v |> Float.sqrt
let normalize v = scale_inv (len v) v
let vx ((x, _, _) : vec3) = x
let vy ((_, y, _) : vec3) = y
let vz ((_, _, z) : vec3) = z

type plane = XY | XZ | YZ

let proj_plane p ((x, y, z) : vec3) =
  match p with XY -> (x, y, 0.0) | XZ -> (x, 0.0, z) | YZ -> (0.0, y, z)

let cross (x1, y1, z1) (x2, y2, z2) =
  let x = (y1 *. z2) -. (z1 *. y2) in
  let y = -1.0 *. ((x1 *. z2) -. (z1 *. x2)) in
  let z = (x1 *. y2) -. (y1 *. x2) in
  (x, y, z)

let are_parallel ?(tol = 1e-7) v1 v2 =
  let c = cross v1 v2 in
  Float.abs (dot c c) < tol

let proj_onto_plane (normal : vec3) (p : vec3) =
  let v = scale (dot normal p /. dot normal normal) normal in
  subv p v
