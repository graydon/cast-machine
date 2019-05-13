(** Writing a functor for type/epxressions **)
open Primitives
open Types
open Types.Print


module type Gradual_Type = sig
    type var = Types.var
    type b = Types.b
    type t = Types.t (* static type *)
    type tau = Types.t (* dynamic type *)
    type subst = Types.subst
    val subst_single : t -> var * t  -> t
    val ceil : tau -> t
    val floor : tau -> t
    val result : tau -> tau -> tau
end

module CDuce_Gradual_Types : Gradual_Type = struct
    type t = Types.t
    type var = Types.var
    type subst = Types.subst
    type b = Types.b
    type tau = t

    let subst_single = CD.Types.Subst.single

    let result tapp targ = app tapp targ

    (* ceil and floor functions *)
    (* warning : these two functions erase type variables with Any ...*)
    let rec ceil t =
     if CD.Types.VarArrow.is_empty (CD.Types.VarArrow.proj t) then 
        subst_single t (v_dyn, any) 
    else
        let (dom,arr) = get t in
        let arr' = List.map (fun l -> 
        List.map (fun (d,r) -> (floor d, ceil r)) l) arr
        in teg (dom, arr')

    and floor t =
    if CD.Types.VarArrow.is_empty (CD.Types.VarArrow.proj t) then 
        subst_single t (v_dyn, empty) 
    else
        let (dom,arr) = get t in
        let arr' = List.map (fun l -> 
        List.map (fun (d,r) -> (ceil d, floor r)) l) arr
        in teg (dom, arr')
end

module type Cast_Expr = sig
    include Gradual_Type
    type p              (* blame labels *)
    type alpha_vector
    type t_vector
    type castkind
    
    type e = 
      | Var of var
      | Cst of b
      | Pair of e * e
      | Let of var * e * e
      | Letrec of var * e * e
      | Lam of tau * var * e
      | App of e * e
      | Cast of e * castkind
      | Succ of e | Pred of e | Fst of e | Snd of e
      | Mult of e * e | Plus of e * e | Minus of e * e | Mod of e * e
      | Ifz of e * e * e
      | Eq of e * e
      | Unit
     type prog =
		| Expr of e
		| Eol
end

module Make_Expr (Init_Type : Gradual_Type) : (Cast_Expr 
  with type castkind := Init_Type.tau * Init_Type.tau) = 
struct
    include Init_Type
    type alpha_vector = Init_Type.var list
    type t_vector = Init_Type.t list
    type p =    (* blame label *)
        [ | `Simple of int
          | `Pos of int * int
          | `Neg of int * int ]
    
    type castkind = tau * tau
    type e = 
      | Var of var
      | Cst of b
      | Pair of e * e
      | Let of var * e * e
      | Letrec of var * e * e
      | Lam of tau * var * e
      | App of e * e
      | Cast of e * castkind
      | Succ of e | Pred of e | Fst of e | Snd of e 
      | Mult of e * e | Plus of e * e | Minus of e * e | Mod of e * e
      | Ifz of e * e * e
      | Eq of e * e
      | Unit
      (* | TwoCast of e * tau * tau  *)
        (* for now no product, let and type abstraction *)
        (* | `Prd of e * e *)
        (* | `Pi1 of e *)
        (* | `Pi2 of e *)
        (* | `TLam of alpha_vector * e *)
        (* | `TApp of e * t_vector *)
      type prog =
		| Expr of e
		| Eol
end

module Eager = struct 
  include Make_Expr(CDuce_Gradual_Types)
  
  module Print = struct
        let pprint_p = function 
            | `Simple n -> string_of_int n
            | `Neg (n, m) | `Pos (n, m) -> Printf.sprintf "(%s, %s)" (string_of_int n) (string_of_int m)

        let pprint_alpha_vector = 
            fun av -> "[" ^ (String.concat " ; " av) ^ "]"

        let pprint_t_vector  = fun tv ->   
            let stv = List.map pprint_t tv in
            String.concat " ; " stv
        
        let sprintf = Printf.sprintf

        let rec pprint_e : e -> string = fun e ->
            let rec aux offset = function
            | Unit -> "()"
            | Var var -> pp_var var
            | Cst b -> pp_b b
            | Pair (e1, e2) ->
                Printf.sprintf "pair (%s, %s)" (pprint_e e1) (pprint_e e2)
            | Mod (e1, e2) ->
                Printf.sprintf "(%s mod %s)" (pprint_e e1) (pprint_e e2)
            | Fst e -> 
                sprintf "fst %s" (pprint_e e)
            | Snd e -> 
                sprintf "snd %s" (pprint_e e)
            | Lam (tau, var, e) ->
                Printf.sprintf "λ [%s] %s . %s"
                (pp_tau tau) (pp_var var) (pprint_e e)  
            | Eq (e1, e2) ->
                Printf.sprintf "%s = %s" (pprint_e e1) (pprint_e e2)
            | Ifz (cond, e1, e2) ->
                Printf.sprintf "%sif %s then %s%selse %s" 
                    (if offset = "" then "\n\t" else offset)
                    (pprint_e cond) (aux offset e1) 
                    (if offset = "" then "\n\t" else "\n" ^ offset) (aux offset e2) 
            | Letrec (x,e1,e2) -> 
                Printf.sprintf "let rec %s = %s in\n%s"
                    (pp_var x) (pprint_e e1) (aux "\t" e2)
            | Let (x, e1, e2) ->
                Printf.sprintf "let %s = %s in\n%s"
                    (pp_var x) (pprint_e e1) (aux "\t" e2)
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
            | Mult (e1, e2) ->
                Printf.sprintf "%s * %s" (pprint_e e1) (pprint_e e2)
            | Plus (e1, e2) ->
                Printf.sprintf "%s + %s" (pprint_e e1) (pprint_e e2)
            | Minus (e1, e2) ->
                Printf.sprintf "%s - %s" (pprint_e e1) (pprint_e e2)
        in aux "" e
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
end

module Symbolic =
struct
    include CDuce_Gradual_Types
    type alpha_vector = var list
    type t_vector = t list
    type p =    (* blame label *)
        [ | `Simple of int
          | `Pos of int * int
          | `Neg of int * int ]
(* hi *)
    type sigma = Id of tau
           | Cast of tau
           | Comp of sigma * sigma
           | App of tau * sigma
           | Dom of sigma
    type castkind = sigma
    type e = 
      | Var of var
      | Cst of b
      | Let of var * e * e
      | Lam of tau * var * e
      | App of e * e
      | Cast of e * castkind
      | Succ of e | Pred of e
    let comp s1 s2 = Comp (s1, s2)

      module Print = struct
        
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
            | Cst b ->   pp_b b
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
                    | Lam _ -> "(%s) 〈%s〉"
                    | Cast _ -> "%s〈%s〉" 
                    | _ -> "%s 〈%s〉") in
                Printf.sprintf s_format (pprint_e e) (pp_tau t)
            | Cast (e, _) -> 
                let s_format : _ format = 
                    (match e with
                    | Lam _ -> "(%s) 〈sigma〉"
                    | Cast _ -> "%s〈sigma〉"
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
end

(* A naive implem of Victor's types and expressions from "Space-efficient [...]" notes *)

(* module SE_Types = struct 
    (* type var = string *)
    type var = string
    type t = 
      [ | `TVar of var
        | `Int 
        | `Bool
        | `Arr of t * t
        | `Or of t * t
        | `And of t * t
        | `Neg of t
        | `Empty ]
    type b = [ `I of int | `B of bool ]
    type tau = 
      [ | `Dyn
        | `TVar of var
        | `Int 
        | `Bool
        | `Arr of tau * tau
        | `Or of tau * tau
        | `And of tau * tau
        | `Neg of tau
        | `Empty ]
end *)


(* module POPL19_Types = struct
    type var = string
    type b = [ `I of int | `B of bool ]
    type t = [
        | `TVar of var
        | `Int 
        | `Bool
        | `Prod of t * t 
        | `Arr of t * t ]
    type tau = [ 
        | `Dyn
        | `TVar of var
        | `Int 
        | `Bool
        | `Prod of tau * tau
        | `Arr of tau * tau ]
end *)

(* module Make_POPL19 (Init_Type : Dynamic_Type) : Cast_Expr = struct
    (** POPL19 types and cast expressions *)
    include Init_Type
    type alpha_vector = Init_Type.var list
    type t_vector = Init_Type.t list
    type p =    (* blame label *)
            [ | `Simple of int
            | `Pos of int * int
            | `Neg of int * int ]
    type e = 
      [ | `Var of var
        | `Cst of b
        | `Lam of tau * tau * var * e
        | `App of e * e
        | `Prd of e * e
        | `Pi1 of e
        | `Pi2 of e
        | `Let of var * e * e
        | `TLam of alpha_vector * e
        | `TApp of e * t_vector
        | `Cast of e * tau * p * tau ]
    (* type v = int *)
    (* type env = int *)
    (* let create () = 0 *)
    end *)

(* module POPL19 = (Make_Cast_Language(POPL19_Types))(Make_POPL19)
module SE = (Make_Cast_Language(SE_Types))(Make_SE) *)


(* module type Cast_Language = sig
    include Cast_Expr
    
    (** DONE : find a way to define the signature of pprint_e and print_e
        that is compatible with the open variant used to define the function
        Here I found a way by adding a single constructor, but I would have like
        to use an "empty open variant" [< ] of which every open variant would be
        a supertype, and therefore by contravariance every function
        [variant -> string] would be a subtype of this type.
        On the other hand, now it becomes impossible to use pprint_e in practice,
        because if I use for example on a list 
            [ `Var "x", `Cst (`I 2) ] 
        which has type
            [> `Var of string | `Cst of b ]
        then this type is not a subtype of [< `Var of string ] *)
      (** Conclusion: switched from variants to constructors, and 
      defined print_e elsewhere after importing this module. *)
end

module type Make_Cast_Expr = (Dynamic_Type -> Cast_Expr) *)