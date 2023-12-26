open Alcotest
open Aoc2023

let call_intersect p1 p2 = Coord3.prisms_intersect ~ignore_z:true p1 p2

let test_coord3 () =
  let p1 = ((0, 0, 0), (2, 2, 0)) in
  let p2 = ((1, 1, 0), (2, 2, 0)) in
  let p3 = ((2, 0, 0), (3, 3, 0)) in
  let p4 = ((1, 0, 0), (3, 2, 0)) in
  let p5 = ((1, 1, 0), (2, 2, 0)) in
  check bool "contained 1" (call_intersect p1 ((0, 0, 0), (1, 1, 1))) true;
  check bool "contained 1 rev" (call_intersect ((0, 0, 0), (1, 1, 1)) p1) true;
  check bool "contained 2" (Coord3.prisms_intersect ~ignore_z:true p1 p2) true;
  check bool "contained 2 rev"
    (Coord3.prisms_intersect ~ignore_z:true p2 p1)
    true;
  check bool "contained 3" (Coord3.prisms_intersect ~ignore_z:true p1 p5) true;
  check bool "contained 3 rev"
    (Coord3.prisms_intersect ~ignore_z:true p5 p1)
    true;

  check bool "tangent X-NX" (Coord3.prisms_intersect ~ignore_z:true p1 p3) false;
  check bool "tangent X-NX rev"
    (Coord3.prisms_intersect ~ignore_z:true p3 p1)
    false;
  let p6 = ((0, 2, 0), (2, 4, 0)) in
  check bool "tangent Y-NY" (Coord3.prisms_intersect ~ignore_z:true p1 p6) false;
  check bool "tangent Y-NY rev"
    (Coord3.prisms_intersect ~ignore_z:true p6 p1)
    false;

  let p7 = ((0, 0, 0), (3, 3, 3)) in
  let p8 = ((1, 1, 0), (2, 2, 0)) in
  let p9 = ((2, 1, 0), (3, 2, 0)) in
  let p10 = ((2, 1, 0), (4, 2, 0)) in
  check bool "contained bigger"
    (Coord3.prisms_intersect ~ignore_z:true p7 p8)
    true;
  check bool "contained bigger rev"
    (Coord3.prisms_intersect ~ignore_z:true p8 p7)
    true;
  check bool "contained with tangent"
    (Coord3.prisms_intersect ~ignore_z:true p7 p9)
    true;
  check bool "contained with tangent rev"
    (Coord3.prisms_intersect ~ignore_z:true p9 p7)
    true;
  check bool "in between sticking out"
    (Coord3.prisms_intersect ~ignore_z:true p7 p10)
    true;
  check bool "in between sticking out rev"
    (Coord3.prisms_intersect ~ignore_z:true p10 p7)
    true;

  let p11 = ((0, 0, 0), (1, 1, 0)) in
  let p12 = ((1, 1, 0), (2, 2, 0)) in
  check bool "check corner"
    (Coord3.prisms_intersect ~ignore_z:true p11 p12)
    false;
  check bool "check corner rev"
    (Coord3.prisms_intersect ~ignore_z:true p12 p11)
    false;

  check bool "colinear intersecting"
    (Coord3.prisms_intersect ~ignore_z:true p1 p4)
    true;
  check bool "colinear intersecting rev"
    (Coord3.prisms_intersect ~ignore_z:true p4 p1)
    true

let () =
  print_endline (Sys.getcwd ());
  run "Testing All Days"
    [ ("Coord3", [ ("prism intersect tester", `Quick, test_coord3) ]) ]
