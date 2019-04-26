module CD = Cduce_lib

let zero = CD.Types.Integer CD.Intervals.V.zero
let one = CD.Types.Integer (CD.Intervals.V.succ CD.Intervals.V.zero)
let mult = CD.Intervals.V.mult
let add = CD.Intervals.V.add
let sub = CD.Intervals.V.sub
let var = CD.Types.var
let any = CD.Types.any
let empty = CD.Types.empty
let arrow = CD.Types.arrow
let neg = CD.Types.neg
let times = CD.Types.times
let cons = CD.Types.cons
let constant = CD.Types.constant
let descr = CD.Types.descr
let equal = CD.Types.equal

let subtype = CD.Types.subtype
let cap = CD.Types.cap
let cup = CD.Types.cup
let diff = CD.Types.diff
let pair t1 t2 = CD.Types.times (cons t1) (cons t2)
let pi1 t = t |> CD.Types.Product.get ~kind:`Normal |> CD.Types.Product.pi1
let pi2 t = t |> CD.Types.Product.get ~kind:`Normal |> CD.Types.Product.pi2
let mk_atom s = s |> CD.Atoms.V.mk_ascii |> CD.Atoms.atom |> CD.Types.atom
let mk_var s = CD.Var.mk ~internal:false s
let mk_arrow t1 t2 = arrow (cons t1) (cons t2)

let collect_vars = CD.Types.collect_vars
let pp_const = CD.Types.Print.pp_const
let succ = CD.Intervals.V.succ
let pred = CD.Intervals.V.pred

let enter_type = CD.Typer.enter_type

(*apply prims*)
let get = CD.Types.Arrow.get
let apply = CD.Types.Arrow.apply
let apply_noarg = CD.Types.Arrow.apply_noarg
let need_arg = CD.Types.Arrow.need_arg
(* TODO is it faster to use apply_noarg when possible *)
(* pro: it doesn't use subtyping *)
(* a check is done to see if possible (List.exists on arrows) *)
let app tapp targ = 
  let (dom,arr) = get tapp in 
  if need_arg (dom,arr) then apply (dom,arr) targ
  else apply_noarg (dom,arr)

let teg (_,arr) =
  List.fold_left (fun cup_acc l -> 
    let cap_arr = List.fold_left (fun cap_acc (s,t) -> cap cap_acc (mk_arrow s t)) any l
    in cup cup_acc cap_arr) empty arr 
    





exception Expression_Syntax_Error
exception Type_Syntax_Error of string
exception Empty_Program


(* from the idea of using an atom to encode dynamic types *)
(*
let dyn_atom = CD.Atoms.V.mk_ascii "Dyn"
let cdyn = CD.Types.descr (cons dyn)
let dyn_fun = arrow (cons dyn) (cons dyn) *)
