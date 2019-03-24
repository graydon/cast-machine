(** Writing a functor for type/epxressions **)

module type Dynamic_Type = sig
    (* type x  *)
    (* variable *)
    type var
    type b
    (* type alpha  *)
    (* type variable *)
    type t (* static type *)
    type tau (* dynamic type *)

    (* val ceil : tau -> t
    val floor : tau -> t *)
end

module type Cast_Expr = sig
    include Dynamic_Type
    type p (* blame labels *)
    type alpha_vector
    type t_vector
    type e (* cast expressions *)
    (* type v *)
end

module type Cast_Language = sig
    include Cast_Expr
    (* other constructs of the language *)
    val is_value : e -> bool
    (* val pprint_e : [< `Var of string ]  -> string  *)
    (** TODO : find a way to define the signature of pprint_e and print_e
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
    (* val print_e : [< `Var of string ]  -> unit *)
end

(* dummy testing of the module signatures *)
(* module Test (Init_Type : Dynamic_Type) : (Dynamic_Type -> Cast_Expr) = 
    functor (DT : Dynamic_Type) ->
    struct
        include DT
        type t_vector = int
        type alpha_vector = int
        type e = int
        type v = int
        type p = int
    end *)

module type Make_Cast_Expr = Dynamic_Type -> Cast_Expr

module POPL19_Types : Dynamic_Type = struct
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
end

module Make_POPL19 (Init_Type : Dynamic_Type) : Cast_Expr = struct
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
    end

(* Victor's types and expressions from "Space-efficient [...]" notes *)

module SE_Types : Dynamic_Type = struct 
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
end

module Make_SE (Init_Type : Dynamic_Type) : Cast_Expr = struct
    include Init_Type
    type alpha_vector = Init_Type.var list
    type t_vector = Init_Type.t list

    type p =    (* blame label *)
           [ | `Simple of int
            | `Pos of int * int
            | `Neg of int * int ]
(* hi *)
    type e = 
      [ | `Var of var
        | `Cst of b
        | `Lam of tau * tau * var * e
        | `App of e * e
        | `Cast of e * tau
        | `TwoCast of e * tau * tau
        (* for now no product, let and type abstraction *)
        (* | `Prd of e * e *)
        (* | `Pi1 of e *)
        (* | `Pi2 of e *)
        (* | `Let of var * e * e *)
        (* | `TLam of alpha_vector * e *)
        (* | `TApp of e * t_vector *)
        ]

    
    (* let create () = Hashtbl.create 50
    let add tbl x y = Hashtbl.add tbl x y *)
end

(* Old values *)
(* 
type u = [ `Lam of tau * tau * var * e]
type v = [ u | `Cst of b | `TwoCast of u * tau * tau ]

let is_u : e -> bool = function
    | `Lam _ | `Cst _ -> true
    | _ -> false

let is_v : e -> bool = fun e ->
    if is_u e then true 
    else match e with
        | `TwoCast (`Lam _, _, _) -> true
        | _ -> false *)


(* Printing functions *)

module Make_Cast_Language (Init_Type : Dynamic_Type) : (Make_Cast_Expr -> Cast_Language) = 
    functor (MCE : Make_Cast_Expr) -> 
    struct
        include MCE(Init_Type)
        let is_value = fun _ -> true
end


let typeof = function
    | `Cst (`I _) -> `Int
    | `Cst (`B _) -> `Bool 
    | `Lam (tau1, tau2, _, _) -> `Arr (tau1, tau2)
    | `TwoCast (`Lam (tau1, tau2, _, _), tau_i, _) -> `And (`Arr (tau1, tau2), tau_i)
    | _ -> failwith "Not a value"


module POPL19 = (Make_Cast_Language(POPL19_Types))(Make_POPL19)
module SE = (Make_Cast_Language(SE_Types))(Make_SE)