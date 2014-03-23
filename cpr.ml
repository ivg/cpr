module Float = Field.Make(Safe.Float)
open Float

type mode = Air | Ground | Intension | TisApprox
type even
type odd

type 'a msg =
  | Even: (int * int) -> even msg
  | Odd : (int * int) -> odd msg


(** Количество битов для кодирования местоположения *)
let bitlength = function
  | TisApprox -> 12. (** TIS-B в сообщении приблизительного местоположения в воздухе *)
  | Intension -> 14. (** намерение *)
  | Air       -> 17. (** ADS-B и TIS-B в воздухе *)
  | Ground    -> 19. (** ADS-B и TIS-B на земле *)

let odd ~x ~y = Odd (x,y)
let even ~x ~y = Even (x,y)

let total_zones = 15.

(** [num_lat_zone lat] число долготных зон для широты [lat].  Формула
    выведена из A.2.6.2.f следующим образом: acos( 1 - (1 -
    cos(%pi/2/N))/ (cos(abs(phi))**2))^-1; trigreduce(%); *)
let num_lat_zone lat = match lat with
  | lat when lat = 0.      -> 59.
  | lat when abs lat > 87. -> 1.
  | lat when abs lat = 87. -> 2.
  | lat -> let sec x = 1. / cos x in
           let nz = total_zones in
           let phi = lat * atan 1. / 45. in
           let a = (sec phi)**2. in
           let b = cos (pi / 2. / nz) in
           let c = a * b - a + 1. in
           floor (2. * pi / acos c)

let abs_degree a = if a < 0. then a + 360. else a
let dlat full i = full / (4. * total_zones - i)
let dlon full rlat i = full / max (num_lat_zone rlat - i) 1.
let round v = floor (v + 0.5)

let encode geo mode i =
  let bits = bitlength mode in
  let full_scale = 2.**bits in
  let scale x dx =
    round (full_scale * (abs_degree x mod dx) / dx) in
  let i = if mode = Intension then 0. else 1. in
  let lat = Geo.degree_of_latitude geo in
  let lon = Geo.degree_of_longitude geo in
  let dlat = dlat 360. i in
  let yz = scale lat dlat in
  let rlat = dlat * (yz / full_scale + floor (lat / dlat)) in
  let dlon = dlon 360. rlat i in
  let xz = scale lon dlon in
  let bits = min bits 17. in
  int_of_float (xz mod 2.**bits),
  int_of_float (yz mod 2.**bits)

let encode_even geo mode =
  let x,y = encode geo mode 0. in
  even ~x ~y

let encode_odd geo mode =
  let x,y = encode geo mode 0. in
  odd ~x ~y

let fmt_of_msg (type t) (msg : t msg) = match msg with
  | Even _ -> 0.
  | Odd  _ -> 1.

let xy_of_msg (type t) (msg : t msg) = match msg with
  | Even (x,y) -> x,y
  | Odd  (x,y) -> x,y

let decode_local refpoint msg mode =
  let xz,yz = xy_of_msg msg in
  let xz = float xz in
  let yz = float yz in
  let bits = min (bitlength mode) 17. in
  let unscale x dx b =
    floor (x / dx) +
      round ((abs_degree x mod dx) / dx - b / (2.**bits)) in
  let lats = Geo.degree_of_latitude refpoint in
  let lons = Geo.degree_of_longitude refpoint in
  let i = fmt_of_msg msg in
  let full = if mode = Ground then 90. else 360. in
  let dlat = dlat full i in
  let j = unscale lats dlat yz in
  let rlat = dlat * (j + yz / (2.**bits)) in
  let dlon = dlon full rlat i in
  let m = unscale lons dlon xz in
  let rlon = dlon * (m + xz / (2.**bits)) in
  Geo.of_degrees rlat rlon


let decode_global even odd =
  let xz0,yz0 = xy_of_msg even in
  let xz1,yz1 = xy_of_msg odd in
  let xz0,yz0,xz1,yz1 = float xz0, float yz0, float xz1, float yz1 in
  let full = 360. in
  let dlat0,dlat1 = dlat full 0., dlat full 1. in
  let full_scale = 2. ** 17. in
  let j = round ((59. * yz0 - 60. * yz1) / full_scale) in
  let rlat dlat yz i = dlat * ((j mod (60. - i)) + yz / full_scale) in
  let rlat0, rlat1 = rlat dlat0 yz0 0., rlat dlat1 yz1 1. in
  let dlon0, dlon1 = dlon full rlat0 0., dlon full rlat1 1. in
  let _NL = num_lat_zone rlat0 in
  let m = round ((xz0 * (_NL - 1.) - xz1 * _NL) / full_scale) in
  let rlon dlon xz i =
    let n = max (_NL - i) 1. in
    dlon * ((m mod n) + xz / full_scale) in
  let rlon0,rlon1 = rlon dlon0 xz0 0., rlon dlon1 xz1 1. in
  Geo.of_degrees (rlat0) (rlon0)
