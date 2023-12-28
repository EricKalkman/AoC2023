open Num

type vec3 = num * num * num
type dir3 = X | Y | Z

let unitx : vec3 = (Int 1, Int 0, Int 0)
let unity : vec3 = (Int 0, Int 1, Int 0)
let unitz : vec3 = (Int 0, Int 0, Int 1)
let zero3 : vec3 = (Int 0, Int 0, Int 0)

let vec3_eq ((x1, y1, z1) : vec3) ((x2, y2, z2) : vec3) =
  eq_num x1 x2 && eq_num y1 y2 && eq_num z1 z2

let vec3_map fn ((x, y, z) : vec3) = (fn x, fn y, fn z)

let vec3_map2 fn ((x1, y1, z1) : vec3) ((x2, y2, z2) : vec3) =
  (fn x1 x2, fn y1 y2, fn z1 z2)

let addv : vec3 -> vec3 -> vec3 = vec3_map2 ( +/ )
let subv : vec3 -> vec3 -> vec3 = vec3_map2 ( -/ )
let scale k : vec3 -> vec3 = vec3_map (( */ ) k)
let scale_inv k : vec3 -> vec3 = vec3_map (fun x -> x // k)

let dot v1 v2 =
  let x, y, z = vec3_map2 ( */ ) v1 v2 in
  x +/ y +/ z

let vx ((x, _, _) : vec3) = x
let vy ((_, y, _) : vec3) = y
let vz ((_, _, z) : vec3) = z

type plane = XY | XZ | YZ

let proj_plane p ((x, y, z) : vec3) =
  match p with XY -> (x, y, Int 0) | XZ -> (x, Int 0, z) | YZ -> (Int 0, y, z)

let cross ((x1, y1, z1) : vec3) ((x2, y2, z2) : vec3) : vec3 =
  let x = (y1 */ z2) -/ (z1 */ y2) in
  let y = Int (-1) */ ((x1 */ z2) -/ (z1 */ x2)) in
  let z = (x1 */ y2) -/ (y1 */ x2) in
  (x, y, z)

let are_parallel v1 v2 =
  let c = cross v1 v2 in
  eq_num (Int 0) @@ dot c c

let floatv_of_vec3 (x, y, z) =
  Num.(float_of_num x, float_of_num y, float_of_num z)

let proj_onto_plane (normal : vec3) (p : vec3) =
  let v = scale (dot normal p // dot normal normal) normal in
  subv p v
