open Syntax
open Syntax.SE_CDuce
open Primitives

module type Type_Print = sig
    type var 
    type t
    type tau 
    type b
    val pprint_t : t -> string
    val pprint_tau : tau -> string
    val pprint_var : var -> string
    val pprint_cst : b -> string
end

module type Type_Print_CD = Type_Print 
    with type t := Types.t 
    and type var := Types.var
    and type tau := tau
    and type b := Types.b

module SE_Print : Type_Print_CD = 
struct 
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
    let pprint_var = CD.Var.ident 
    let pprint_tau = pprint_t
    let pprint_cst c = 
        CD.Types.Print.pp_const (Format.str_formatter) c; Format.flush_str_formatter ()
end

(** Useless functor; but gives the beginning of current Print modules *)
module Make_Print = functor (Print : Type_Print_CD) -> struct
    include Print
    let pprint_p = function 
        | `Simple n -> string_of_int n
        | `Neg (n, m) | `Pos (n, m) -> Printf.sprintf "(%s, %s)" (string_of_int n) (string_of_int m)

    let pprint_alpha_vector = 
        fun av -> "[" ^ (String.concat " ; " av) ^ "]"

    let pprint_t_vector  = fun tv ->   
        let stv = List.map pprint_t tv in
        String.concat " ; " stv

    let (putain : e -> string) = function
        | Var v -> "bordel"
        | _ -> ""

    let rec (pprint_e : SE_CDuce.e -> string) = function
        | Var var -> pprint_var var
        | Cst b -> pprint_cst b
        | Lam (tau1, tau2, var, e) -> 
            Printf.sprintf "(λ %s . %s) : %s 🡒  %s" (pprint_var var) (pprint_e e) (pprint_tau tau1) (pprint_tau tau2)
        | App (e1, e2) -> 
            Printf.sprintf "(%s) %s" (pprint_e e1) (pprint_e e2)
        | Cast (e, (tau1, tau2)) ->
            let s_format : _ format = 
                (match e with
                | Lam _ -> "(%s) 〈%s, %s 〉" (* careful: influences the variant type *)
                | Cast _ -> "%s〈%s, %s 〉" (* careful: influences the variant type *)
                | _ -> "%s 〈%s, %s 〉") in
            Printf.sprintf s_format (pprint_e e) (pprint_tau tau1) (pprint_tau tau2)
        (* | `Prd (e1, e2) ->
            Printf.sprintf "(%s, %s)" (pprint_e e1) (pprint_e e2)
        | `Pi1 e -> 
            Printf.sprintf "π_1 %s" (pprint_e e)
        | `Pi2 e ->
            Printf.sprintf "π_1 %s" (pprint_e e)
        | `Let (var, e1, e2) ->
            Printf.sprintf "let %s = %s in %s" (pprint_var var) (pprint_e e1) (pprint_e e2)
        | `TLam (av, e) ->
            Printf.sprintf "Λ %s . %s" (pprint_alpha_vector av) (pprint_e e)
        | `TApp (e, tv) ->
            Printf.sprintf "(%s) [%s]" (pprint_e e) (pprint_t_vector tv) *)

    let print_e = function e -> print_string (pprint_e e)
    let print_t = fun t -> print_string (pprint_t t)
end

module Print = Make_Print(SE_Print)
include Print