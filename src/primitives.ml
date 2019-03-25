(* This file was copied from Tommaso's setvariants *)

(* open Utils *)
open Types

(* module CD = Cduce_lib *)

let var = CD.Types.var
let any = CD.Types.any
let empty = CD.Types.empty
let arrow = CD.Types.arrow
let times = CD.Types.times
let cons = CD.Types.cons
let descr = CD.Types.descr
let dom = CD.Types.Arrow.domain
let subtype = CD.Types.subtype
let cap = CD.Types.cap
let cup = CD.Types.cup
let diff = CD.Types.diff
let mk_atom s = s |> CD.Atoms.V.mk_ascii |> CD.Atoms.atom |> CD.Types.atom
let mk_var s = CD.Var.mk ~internal:false s |> var
let collect_vars = CD.Types.collect_vars

let vardyn = `Var (fresh_dyn_var ())
let vardyn2 = `Var (fresh_dyn_var ())
(* let dyn = mk_atom "Dyn"
let dyn_atom = CD.Atoms.V.mk_ascii "Dyn"
let cdyn = CD.Types.descr (cons dyn)
let dyn_fun = arrow (cons dyn) (cons dyn) *)
