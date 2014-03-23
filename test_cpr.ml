open OUnit
open Cpr

let points =
  List.map (fun (lat,lon) -> Geo.of_degrees lat lon)
  [
    0.0      ,0.0;
    56.4     ,37.9;
    (-56.4)  ,37.9;
    56.4     ,(-37.9);
    (-56.4)  ,(-37.9);
    5.       ,5.;
    5.       ,~-.5.;
    ~-.5.    ,5.;
    ~-.5.    ,~-.5.;
    90.      ,0.;
    90.      ,180.;
    90.      ,(-180.);
    55.55    ,22.22;
    (-55.55) ,(-22.22);
    (-22.22) ,55.55;
    22.22    ,(-55.55);
    55.7955  ,37.7064;
    37.70    , 55.79
  ]

let equal p0 p1 mode =
  let d = match mode with
    | Air       -> 5.0
    | Ground    -> 1.25
    | Intension -> 41.0
    | TisApprox -> 164.0 in
  Geo.distance p0 p1 < d

let local mode point  =
  let x0,y0 = encode point Even mode in
  let refpoint = Geo.destination (Angle.of_degree 42.0) 0.0 point in
  let dl = decode_local refpoint (x0,y0) Even mode in
  TestCase (fun () -> assert_bool "local" (equal point dl mode))

let global mode point =
  let x0,y0 = encode point Even mode in
  let point1 =
    Geo.destination (Angle.of_degree 42.0) 200.0 point in
  let x1,y1 = encode point1 Odd mode in
  let dg = decode_global (x0,y0) (x1,y1) mode in
  TestCase (fun () -> assert_bool "global" (equal point dg mode))

let test f mode = List.map (f mode) points

let suite =
  "codec_cpr" >::: [
    "Local" >::: [
      "Air"        >::: test local  Air;
      "Ground"     >::: test local  Ground;
      "Intension"  >::: test local  Intension;
      "TisApprox"  >::: test local  TisApprox;
    ];
    "Global" >::: [
      "Air"       >::: test global Air;
    ]
  ]
