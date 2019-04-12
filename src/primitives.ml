open Types

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
let get = CD.Types.Arrow.get

(* important: in cduce, dom of any returns empty, 
   which we do not want *)
let dom t = 
    if t = any then any
    else CD.Types.Arrow.domain (get t)
    
let subtype = CD.Types.subtype
let app tapp targ = CD.Types.Arrow.apply (get tapp) targ
let cap = CD.Types.cap
let cup = CD.Types.cup
let diff = CD.Types.diff
let mk_atom s = s |> CD.Atoms.V.mk_ascii |> CD.Atoms.atom |> CD.Types.atom
let mk_var s = CD.Var.mk ~internal:false s
let mk_arrow t1 t2 = arrow (cons t1) (cons t2)
let collect_vars = CD.Types.collect_vars
let pp_const = CD.Types.Print.pp_const
let succ = CD.Intervals.V.succ
let pred = CD.Intervals.V.pred


exception Expression_Syntax_Error
exception Type_Syntax_Error of string
exception Empty_Program



(* transform a string into a cduce type *)
let parse_t str = 
    try 
    str |> Str.global_substitute (Str.regexp_string "?")
            (fun _ -> " " ^ fresh_dyn_id () ^ " ") 
        |> Stream.of_string |> CD.Parser.pat 
        |> CD.Typer.typ CD.Builtin.env |> CD.Types.descr
    with _ -> raise (Type_Syntax_Error str)

let parse_cst str = 
        try
        str |> Stream.of_string |> CD.Parser.expr
            |> CD.Typer.type_expr CD.Builtin.env |> fst
            |> CD.Compile.compile_eval_expr CD.Compile.empty_toplevel
            |> CD.Value.inv_const
        with _ -> raise Expression_Syntax_Error

let split = Str.split (Str.regexp " +")

(* some types *)
let arr_t = parse_t "Arrow"
let narr_t = neg (arr_t)

let x = List.mem

let process_funpat f =
    let fl = split f in 
    let fn = String.concat "" (List.tl (fl)) in
	parse_t fn


let qmark () = var (fresh_dyn_var ())
let qm () = cons (qmark ())
let qfun () = arrow (qm ()) (qm ())

(* from the idea of using an atom to encode dynamic types *)
(*
let dyn_atom = CD.Atoms.V.mk_ascii "Dyn"
let cdyn = CD.Types.descr (cons dyn)
let dyn_fun = arrow (cons dyn) (cons dyn) *)
