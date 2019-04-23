open Primitives

type t = CD.Types.t
type var = CD.Var.t
type varset = CD.Var.Set.t
type subst = CD.Types.Subst.t
type b = CD.Types.const
type tau = CD.Types.t

let fresh_var () =
  let n = Oo.id (object end) in
  CD.Var.mk ~internal:false (Printf.sprintf "a%04d" n)

let fresh_dyn_id () =
  let n = Oo.id (object end) in
  Printf.sprintf "'d%04d" n

let fresh_dyn_var () =
  let n = Oo.id (object end) in
  CD.Var.mk ~internal:false (Printf.sprintf "d%04d" n)

let fresh_var_type () = CD.Types.var (fresh_var ())

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
                (List.map (fun v -> (v, var (mk_var "?"))) (CD.Var.Set.get dv))
            in CD.Types.Print.string_of_type t'
    let pp_var = CD.Var.ident 
    let pp_tau = pprint_t
    let pp_b c = 
        CD.Types.Print.pp_const (Format.str_formatter) c; Format.flush_str_formatter ()
end

(* type environment during evaluation *)
let env = CD.Builtin.env

(* adding my builtin types *)
let mk_ident = CD.Ident.U.mk
let ns_empty = CD.Ns.empty

(* bottom type *)
let t_bot = mk_atom "Bottom"

let n = (ns_empty, mk_ident "Bottom")
let env = enter_type (CD.Ident.ident n) t_bot env

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
            (fun _ -> " " ^ fresh_dyn_id () ^ " ") 
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

(* some types *)
let arr_t = parse_t "Arrow"
let narr_t = neg (arr_t)


let split = Str.split (Str.regexp " +")


let process_funpat f =
    let fl = split f in 
    let fn = String.concat "" (List.tl (fl)) in
	parse_t fn


let qmark () = var (fresh_dyn_var ())
let qm () = cons (qmark ())
let qfun () = arrow (qm ()) (qm ())