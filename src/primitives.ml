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
let constant = CD.Types.constant
let descr = CD.Types.descr
let get = CD.Types.Arrow.get
let dom t = CD.Types.Arrow.domain (get t)
let subtype = CD.Types.subtype
let apply tapp targ = CD.Types.Arrow.apply (get tapp) targ
let cap = CD.Types.cap
let cup = CD.Types.cup
let diff = CD.Types.diff
let mk_atom s = s |> CD.Atoms.V.mk_ascii |> CD.Atoms.atom |> CD.Types.atom
let mk_var s = CD.Var.mk ~internal:false s
let collect_vars = CD.Types.collect_vars

let pp_const = CD.Types.Print.pp_const

(* let vardyn = `Var (fresh_dyn_var ())
let vardyn2 = `Var (fresh_dyn_var ()) *)

let qmark () = var (fresh_dyn_var ())
let qm () = cons (qmark ())
let qfun () = arrow (qm ()) (qm ())


(*
let dyn_atom = CD.Atoms.V.mk_ascii "Dyn"
let cdyn = CD.Types.descr (cons dyn)
let dyn_fun = arrow (cons dyn) (cons dyn) *)