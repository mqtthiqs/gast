open Sexp

type t =
  | A of t * t list
  | B of int
  | C

let v = (A(C,[B 33;C;A(B 43,[])]));;


