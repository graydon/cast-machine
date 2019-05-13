module CD = Cduce_lib

let zero = CD.Types.Integer CD.Intervals.V.zero
let one = CD.Types.Integer (CD.Intervals.V.succ CD.Intervals.V.zero)
let mult = CD.Intervals.V.mult
let add = CD.Intervals.V.add
let (mod) = CD.Intervals.V.modulo
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

let show_arr (_,arr) = 
      List.map (fun l -> List.map (fun (a,b) -> Printf.printf "%s, %s\n" 
      (CD.Types.Print.string_of_type a) (CD.Types.Print.string_of_type b)) l) arr

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
  if not (subtype targ dom) then empty
  else 
  (* let () = print_endline "debug" in
  let _ = show_arr (dom,arr) in  *)
  if need_arg (dom,arr) then apply (dom,arr) targ
  else apply_noarg (dom,arr)

let teg (_,arr) =
  List.fold_left (fun cup_acc l -> 
    let cap_arr = List.fold_left (fun cap_acc (s,t) -> cap cap_acc (mk_arrow s t)) any l
    in cup cup_acc cap_arr) empty arr

let fresh_var () =
  let n = Oo.id (object end) in
  CD.Var.mk ~internal:false (Printf.sprintf "a%04d" n)

let fresh_dyn_id () =
  let n = Oo.id (object end) in
  Printf.sprintf "d%04d" n

let fresh_dyn_var () =
  let n = Oo.id (object end) in
  CD.Var.mk ~internal:false (Printf.sprintf "d%04d" n)

let fresh_dyn () = var (fresh_dyn_var ())

let fresh_var_type () = CD.Types.var (fresh_var ())

    

exception Expression_Syntax_Error
exception Type_Syntax_Error of string
exception Empty_Program


(* from the idea of using an atom to encode dynamic types *)
(*
let dyn_atom = CD.Atoms.V.mk_ascii "Dyn"
let cdyn = CD.Types.descr (cons dyn)
let dyn_fun = arrow (cons dyn) (cons dyn) *)
