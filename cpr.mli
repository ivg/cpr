(** Compact Coordinates Report coder/decoder.

    Module is implemented according to ICAO specification Doc 9871
    AN/464 section A.2.6.
  *)

type mode = Air | Ground | Intension | TisApprox
type odd
type even

type 'a msg = private
  | Even: (int * int) -> even msg
  | Odd : (int * int) -> odd msg


val odd: x:int -> y:int -> odd msg
val even: x:int -> y:int -> even msg

(** [codec mode] creates a coder/decoder using [mode].
    see A.2.6.1, A.2.6.2
*)
(** [encode point fmt] encodes [point] using [format] according to
    A.2.6.3 *)
val encode_even: Geo.t -> mode -> even msg
val encode_odd: Geo.t -> mode -> odd msg

(** [decode_local ref xz yz fmt] decodes [msg] and [yz] to localy
    unambigous coordinate using reference point [ref] and format
    [fmt]. See A.2.6.4-A.2.6.6 *)
val decode_local: Geo.t -> 'a msg -> mode -> Geo.t

(** [decode_global xz0 yz0 xz1 yz1 mode fmt_last] decodes an even [xz0],[yz0] and
    odd [xz1], [yz1] to globaly unambigous coordinates.
    See A.2.6.7 *)
val decode_global: even msg -> odd msg -> Geo.t
