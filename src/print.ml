open Syntax
open Primitives

(* module type Type_Print = sig
    type var 
    type t
    type tau 
    type b
    val pprint_t : t -> string
    val pp_tau : tau -> string
    val pp_var : var -> string
    val pp_b : b -> string
end *)

(* module type Type_Print_CD = Type_Print 
    with type t := Types.t 
    and type var := Types.var
    and type tau := Types.tau
    and type b := Types.b *)

module SE_Print = 
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
    let pp_var = CD.Var.ident 
    let pp_tau = pprint_t
    let pp_b c = 
        CD.Types.Print.pp_const (Format.str_formatter) c; Format.flush_str_formatter ()
end

(** Useless functor; but gives the beginning of current Print modules *)
module Print = struct
    open SE_CDuce
    include SE_Print
    let pprint_p = function 
        | `Simple n -> string_of_int n
        | `Neg (n, m) | `Pos (n, m) -> Printf.sprintf "(%s, %s)" (string_of_int n) (string_of_int m)

    let pprint_alpha_vector = 
        fun av -> "[" ^ (String.concat " ; " av) ^ "]"

    let pprint_t_vector  = fun tv ->   
        let stv = List.map pprint_t tv in
        String.concat " ; " stv

    let (putain : e -> string) = function
        | Var _ -> "bordel"
        | _ -> ""

    let rec (pprint_e : e -> string) = function
        | Var var -> pp_var var
        | Cst b -> pp_b b
        | Lam (tau, var, e) -> 
            Printf.sprintf "λ [%s] %s . %s" (pp_tau tau) (pp_var var) (pprint_e e) 
        | Let (x, e1, e2) ->
            Printf.sprintf "let %s = %s in %s"
                (pp_var x) (pprint_e e1) (pprint_e e2)
        | App (e1, e2) -> 
            let s_format : _ format =
                (match e2 with
                | Lam _ | Cast _ -> "(%s) (%s)"
                | _ ->     "(%s) %s") in
            Printf.sprintf s_format (pprint_e e1) (pprint_e e2)
        | Cast (e, (tau1, tau2)) ->
            let s_format : _ format = 
                (match e with
                | Lam _ -> "(%s) 〈%s, %s〉" (* careful: influences the variant type *)
                | Cast _ -> "%s〈%s, %s〉" (* careful: influences the variant type *)
                | _ -> "%s 〈%s, %s〉") in
            Printf.sprintf s_format (pprint_e e) (pp_tau tau1) (pp_tau tau2)
        | Succ (e) ->
            Printf.sprintf "succ %s" (pprint_e e)
        | Pred (e) ->
            Printf.sprintf "pred %s" (pprint_e e)
        (* | `Prd (e1, e2) ->
            Printf.sprintf "(%s, %s)" (pprint_e e1) (pprint_e e2)
        | `Pi1 e -> 
            Printf.sprintf "π_1 %s" (pprint_e e)
        | `Pi2 e ->
            Printf.sprintf "π_1 %s" (pprint_e e)
        | `Let (var, e1, e2) ->
            Printf.sprintf "let %s = %s in %s" (pp_var var) (pprint_e e1) (pprint_e e2)
        | `TLam (av, e) ->
            Printf.sprintf "Λ %s . %s" (pprint_alpha_vector av) (pprint_e e)
        | `TApp (e, tv) ->
            Printf.sprintf "(%s) [%s]" (pprint_e e) (pprint_t_vector tv) *)

    let print_e = function e -> print_string (pprint_e e)
    let print_t = fun t -> print_string (pprint_t t)
end


(** Useless functor; but gives the beginning of current Print modules *)
module Print_Symbolic = struct
    open SE_CDuce_Symbolic
    include SE_Print
    let pprint_p = function 
        | `Simple n -> string_of_int n
        | `Neg (n, m) | `Pos (n, m) -> Printf.sprintf "(%s, %s)" (string_of_int n) (string_of_int m)

    let pprint_alpha_vector = 
        fun av -> "[" ^ (String.concat " ; " av) ^ "]"

    let pprint_t_vector  = fun tv ->   
        let stv = List.map pprint_t tv in
        String.concat " ; " stv

    let rec (pprint_e : e -> string) = function
        | Var var -> pp_var var
        | Cst b -> pp_b b
        | Lam (tau, var, e) -> 
            Printf.sprintf "(λ %s . %s) : %s" (pp_var var) (pprint_e e) (pp_tau tau) 
        | Let (x, e1, e2) ->
            Printf.sprintf "let %s = %s in %s"
                (pp_var x) (pprint_e e1) (pprint_e e2)
        | App (e1, e2) -> 
            Printf.sprintf "(%s) %s" (pprint_e e1) (pprint_e e2)
        | Cast (e, (Cast t | Id t)) ->
            let s_format : _ format = 
                (match e with
                | Lam _ -> "(%s) 〈%s〉" (* careful: influences the variant type *)
                | Cast _ -> "%s〈%s〉" (* careful: influences the variant type *)
                | _ -> "%s 〈%s〉") in
            Printf.sprintf s_format (pprint_e e) (pp_tau t)
        | Cast (e, _) -> 
            let s_format : _ format = 
                (match e with
                | Lam _ -> "(%s) 〈sigma〉" (* careful: influences the variant type *)
                | Cast _ -> "%s〈sigma〉" (* careful: influences the variant type *)
                | _ -> "%s 〈sigma〉") in
            Printf.sprintf s_format (pprint_e e)
        | Succ (e) ->
            Printf.sprintf "succ %s" (pprint_e e)
        | Pred (e) ->
            Printf.sprintf "pred %s" (pprint_e e)

        (* | `Prd (e1, e2) ->
            Printf.sprintf "(%s, %s)" (pprint_e e1) (pprint_e e2)
        | `Pi1 e -> 
            Printf.sprintf "π_1 %s" (pprint_e e)
        | `Pi2 e ->
            Printf.sprintf "π_1 %s" (pprint_e e)
        | `Let (var, e1, e2) ->
            Printf.sprintf "let %s = %s in %s" (pp_var var) (pprint_e e1) (pprint_e e2)
        | `TLam (av, e) ->
            Printf.sprintf "Λ %s . %s" (pprint_alpha_vector av) (pprint_e e)
        | `TApp (e, tv) ->
            Printf.sprintf "(%s) [%s]" (pprint_e e) (pprint_t_vector tv) *)

    let print_e = function e -> print_string (pprint_e e)
    let print_t = fun t -> print_string (pprint_t t)
end

include Print
include Print_Symbolic