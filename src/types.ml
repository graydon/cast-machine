open Primitives
open Benchmark

type t = CD.Types.t
type var = CD.Var.t
type varset = CD.Var.Set.t
type subst = CD.Types.Subst.t
type b = CD.Types.const
type tau = CD.Types.t

module Print = struct 
    let pprint_t t = 
        (* hacky: gives ident [?] to all dynamic type variables 
        in order to print them *)
        let t' =
            let av = CD.Types.all_vars t in
            let dv = 
                CD.Var.Set.filter (fun v -> 
                    (CD.Var.ident v).[0] = 'd') av
            in
            CD.Types.Subst.full_list t 
                (List.map (fun v -> (v, var (mk_var "?"))) (CD.Var.Set.get dv)) in 
        CD.Types.Print.string_of_type t'
    let pp_var = CD.Var.ident 
    let pp_tau = pprint_t
    let pp_b c = 
        CD.Types.Print.pp_const (Format.str_formatter) c; Format.flush_str_formatter ()

    let show_arr (_,arr) = 
      List.map (fun l -> List.map (fun (a,b) -> Printf.printf "%s, %s\n" (pp_tau a) (pp_tau b)) l) arr
end

(* type environment during evaluation *)
let env = CD.Builtin.env

(* adding my builtin types *)
let mk_ident = CD.Ident.U.mk
let ns_empty = CD.Ns.empty

(* let _ = List.map 
  (fun (n,t) -> 
    let n = (CD.Ns.empty, CD.Ident.U.mk n) in
      (* CD.Types.Print.register_global "" n t; *)
      CD.Typer.enter_type (CD.Ident.ident n) t env)
  Benchmark.builtins *)

(* qmark type *)
let s_dyn = fresh_dyn_id ()
let v_dyn = CD.Var.mk ~internal:false s_dyn
let t_dyn = var v_dyn

(* let n = (ns_empty, mk_ident "Dyn")
let env = enter_type (CD.Ident.ident n) t_dyn env *)

(* bottom type *)
let t_bot = mk_atom "Bottom"

let n = (ns_empty, mk_ident "Bottom")
let env = enter_type (CD.Ident.ident n) t_bot env

(* add my builtins *)
let env = List.fold_left 
  (fun e s -> add_typedefs e s) env builtins 

let is_bottom t = equal t t_bot

let is_qmark t = 
  try let (v, b) = CD.Types.Variable.extract t in
  b && (CD.Var.ident v).[0] = 'd'
  with Invalid_argument _ -> false

(* important: in cduce, dom of any returns empty, 
   which we do not want *)
let dom t = 
    if t = any || is_qmark t then any
    else let (d,arr) = get t in
    if arr = [] || arr = [[]] then t_bot
    else d

(* transform a string into a cduce type *)
let parse_t str = 
    try 
    str |> Str.global_substitute (Str.regexp_string "?")
            (fun _ -> "'" ^ s_dyn ^ " ") 
        |> Stream.of_string |> CD.Parser.pat 
        |> CD.Typer.typ env |> CD.Types.descr
    with _ -> raise (Type_Syntax_Error str)

let parse_cst str = 
        try
        str |> Stream.of_string |> CD.Parser.expr
            |> CD.Typer.type_expr env |> fst
            |> CD.Compile.compile_eval_expr CD.Compile.empty_toplevel
            |> CD.Value.inv_const
        with _ -> raise Expression_Syntax_Error
(* 
let parse_typedecl s = 
  try  *)

(* some types *)
let t_arr = parse_t "Arrow"
let t_not_arr = neg (t_arr)
let t_int = parse_t "Int"
let t_bool = parse_t "Bool"

let split = Str.split (Str.regexp " +")

let process_funpat f =
    let fl = split f in 
    let fn = String.concat "" (List.tl (fl)) in
	parse_t fn

let qmark () = t_dyn
let qm () = cons (t_dyn)
let qfun () = mk_arrow (qmark ()) (qmark ())
