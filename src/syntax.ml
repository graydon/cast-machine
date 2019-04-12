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
    val subst : t -> (var * t) list -> t
    val ceil : tau -> t
    val floor : tau -> t
    val apply : tau -> tau -> tau
end

module CDuce_Gradual_Types : Gradual_Type = struct
    type t = Types.t
    type var = Types.var
    (* type varset = Types.varset *)
    type subst = Types.subst
    type b = Types.b (* surely false *)
    (* type subst = CD.Types.Subst.t *)
    (* Idée: à la manière des schemes de Tommaso, 
    implémenter les types graduels comme des types
    t possédant des variables de types assignées à `Dyn 
    Je choisis cette version par rapport à celle utilisant 
    des atomes `Dyn, parce que CDuce permet de récupérer
    la variance des variables de type facilement, et aussi
    les variables de type permettent de les représenter de 
    manière distincte (avec des [fresh_var ()]) *)
    type tau = t


    let subst = CD.Types.Subst.full_list
    
    let refresh_grad t =
      let _, _, _, all = collect_vars t in
      subst t
          (List.map 
            (fun v -> (v, qmark()))
            (CD.Var.Set.get 
              (CD.Var.Set.filter (fun v ->
                (CD.Var.ident v).[0] = 'd') all)))

    let subst_grad t trep = 
      let _, _, _, all = collect_vars t in
      subst t
        (List.map 
          (fun v -> (v, trep))
          (CD.Var.Set.get 
            (CD.Var.Set.filter (fun v ->
              (CD.Var.ident v).[0] = 'd') all)))

    let apply tapp targ = 
      let qm = qmark () in
      let tapp' = subst_grad tapp qm in 
      let targ' = subst_grad targ qm in
      let tres = app tapp' targ' in
      refresh_grad tres



    (* let subst_single = CD.Types.Subst.single *)
    (* let variance = CD.Types.Variable.variance *)
    (* let pp_type = CD.Types.Print.pp_type *)
    (* let apply = CD.Types.Arrow.apply *)
      (* [apply t1 t2 computes [t1 \circ t2] *)
    (* let get = CD.Types.Arrow.get *)
    (* let is_arrow t = subtype t CD.Types.Arrow.any *)

    let ceil t =
      let _, pos, neg, _ = collect_vars t in
      (* check if the first letter of the ident 
      is 'd', because then it is  gradual typevar *)
      let t' = 
      subst t 
        (List.map 
          (fun v -> (v, any))
          (CD.Var.Set.get 
            (CD.Var.Set.filter (fun v ->
              (CD.Var.ident v).[0] = 'd') pos)))
      in
      subst t'
        (List.map 
          (fun v -> (v, empty))
          (CD.Var.Set.get 
            (CD.Var.Set.filter (fun v ->
              (CD.Var.ident v).[0] = 'd') neg)))
          
    let floor t =
      let _, pos, neg, _ = collect_vars t in
      (* check if the first letter of the ident 
      is 'd', because then it is a gradual typevar *)
      let t' = 
      subst t 
        (List.map 
          (fun v -> (v, any))
          (CD.Var.Set.get 
            (CD.Var.Set.filter (fun v ->
              (CD.Var.ident v).[0] = 'd') neg)))
      in
      subst t'
        (List.map 
          (fun v -> (v, empty))
          (CD.Var.Set.get 
            (CD.Var.Set.filter (fun v ->
              (CD.Var.ident v).[0] = 'd') pos)))

    
end

module type Cast_Expr = sig
    include Gradual_Type
    type p (* blame labels *)
    type alpha_vector
    type t_vector
    type castkind
    
    type e = 
      | Var of var
      | Cst of b
      | Let of var * e * e
      | Lamrec of var * tau * var * e
      | Lam of tau * var * e
      | App of e * e
      | Cast of e * castkind
      | Succ of e | Pred of e
      | Mult of e * e | Plus of e * e | Minus of e * e
      | Ifz of e * e * e
      | Eq of e * e
      | Unit
      (* | TwoCast of e * tau * tau  *)
    (* type v *)
     type prog =
		| Expr of e
		| Eol
end

module Make_SE (Init_Type : Gradual_Type) : (Cast_Expr 
  with type castkind := Init_Type.tau * Init_Type.tau) = 
struct
    include Init_Type
    type alpha_vector = Init_Type.var list
    type t_vector = Init_Type.t list
    type p =    (* blame label *)
        [ | `Simple of int
          | `Pos of int * int
          | `Neg of int * int ]
(* hi *)
    type castkind = tau * tau
    type e = 
      | Var of var
      | Cst of b
      | Let of var * e * e
      | Lamrec of var * tau * var * e
      | Lam of tau * var * e
      | App of e * e
      | Cast of e * castkind
      | Succ of e | Pred of e
      | Mult of e * e | Plus of e * e | Minus of e * e
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
  include Make_SE(CDuce_Gradual_Types)
  
  module Print = struct
        let pprint_p = function 
            | `Simple n -> string_of_int n
            | `Neg (n, m) | `Pos (n, m) -> Printf.sprintf "(%s, %s)" (string_of_int n) (string_of_int m)

        let pprint_alpha_vector = 
            fun av -> "[" ^ (String.concat " ; " av) ^ "]"

        let pprint_t_vector  = fun tv ->   
            let stv = List.map pprint_t tv in
            String.concat " ; " stv


        let rec pprint_e : e -> string = function
            | Unit -> "()"
            | Var var -> pp_var var
            | Cst b -> pp_b b
            | Lam (tau, var, e) ->
                Printf.sprintf "λ [%s] %s . %s"
                (pp_tau tau) (pp_var var) (pprint_e e)  
            | Lamrec (f, tau, var, e)  -> 
                Printf.sprintf "λ_%s [%s] %s . %s"
                (pp_var f) (pp_tau tau) (pp_var var) (pprint_e e) 
            | Eq (e1, e2) ->
                Printf.sprintf "%s = %s" (pprint_e e1) (pprint_e e2)
            | Ifz (cond, e1, e2) ->
                Printf.sprintf "if %s then %s else %s"
                    (pprint_e cond) (pprint_e e1) (pprint_e e2)
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
            | Mult (e1, e2) ->
                Printf.sprintf "%s * %s" (pprint_e e1) (pprint_e e2)
            | Plus (e1, e2) ->
                Printf.sprintf "%s + %s" (pprint_e e1) (pprint_e e2)
            | Minus (e1, e2) ->
                Printf.sprintf "%s - %s" (pprint_e e1) (pprint_e e2)
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