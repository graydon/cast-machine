open Types

let zero = CD.Intervals.V.zero
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
let mk_arrow t1 t2 = arrow (cons t1) (cons t2)
let collect_vars = CD.Types.collect_vars
let pp_const = CD.Types.Print.pp_const
let succ = CD.Intervals.V.succ
let pred = CD.Intervals.V.pred

exception Expression_Syntax_Error
exception Type_Syntax_Error

(* transform a string into a cduce type *)
let parse_t str = 
    try 
    str |> Str.global_substitute (Str.regexp_string "?")
            (fun _ -> fresh_dyn_id ()) 
        |> Stream.of_string |> CD.Parser.pat 
        |> CD.Typer.typ CD.Typer.empty_env |> CD.Types.descr
    with _ -> raise Type_Syntax_Error

let parse_cst str = 
        try
        str |> Stream.of_string |> CD.Parser.expr
            |> CD.Typer.type_expr CD.Typer.empty_env |> fst
            |> CD.Compile.compile_eval_expr CD.Compile.empty_toplevel
            |> CD.Value.inv_const
        with _ -> raise Expression_Syntax_Error

let qmark () = var (fresh_dyn_var ())
let qm () = cons (qmark ())
let qfun () = arrow (qm ()) (qm ())

(* from the idea of using an atom to encode dynamic types *)
(*
let dyn_atom = CD.Atoms.V.mk_ascii "Dyn"
let cdyn = CD.Types.descr (cons dyn)
let dyn_fun = arrow (cons dyn) (cons dyn) *)
