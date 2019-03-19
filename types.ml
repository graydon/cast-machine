type alpha = string

(** Writing a functor for type/epxressions **)

module type Dynamic_Type = sig
    type x (* variable *)
    type b
    type alpha (* type variable *)
    type t (* static type *)
    type tau (* dynamic type *)
end

module type Cast_Expr = sig
    include Dynamic_Type
    type p (* blame labels *)
    type alpha_vector
    type t_vector
    type e (* cast expressions *)
    type v (* values *)
end

module type Cast_Language = sig
    include Cast_Expr
    (* other constructs of the language *)
    val is_value : e -> bool
    val pprint_e : [< `Var of string ]  -> string 
    (** TODO : find a way to define the signature of pprint_e and print_e
        that is compatible with the open variant used to define the function
        Here I found a way by adding a single cosntructor, but I would have like
        to use an "empty open variant" [< ] of which every open variant would be
        a supertype, and therefore by contravariance every function
        [variant -> string] would be a subtype of this type *)
    val print_e : [< `Var of string ]  -> unit
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
    type alpha = string
    type x = string
    type b = [ `I of int | `B of bool ]
    type t = [
        | `TVar of alpha
        | `Int 
        | `Bool
        | `Prod of t * t 
        | `Arr of t * t ]
    type tau = [ 
        | `Dyn
        | `TVar of alpha
        | `Int 
        | `Bool
        | `Prod of tau * tau
        | `Arr of tau * tau ]
end

module Make_POPL19 (Init_Type : Dynamic_Type) : Cast_Expr = struct
    (** POPL19 types and cast expressions *)
    include Init_Type
    type alpha_vector = Init_Type.alpha list
    type t_vector = Init_Type.t list
    type p =    (* blame label *)
            [ | `Simple of int
            | `Pos of int * int
            | `Neg of int * int ]
    type e = 
      [ | `Var of x
        | `Cst of b
        | `Lam of tau * tau * x * e
        | `App of e * e
        | `Prd of e * e
        | `Pi1 of e
        | `Pi2 of e
        | `Let of x * e * e
        | `TLam of alpha_vector * e
        | `TApp of e * t_vector
        | `Cast of e * tau * p * tau ]
    type v = int
    end

(* Victor's types and expressions from "Space-efficient [...]" notes *)

module SE_Types : Dynamic_Type = struct 
    type alpha = string
    type x = string
    type b = [ `I of int | `B of bool ]

    type t = 
      [ | `TVar of alpha
        | `Int 
        | `Bool
        | `Arr of t * t
        | `Or of t * t
        | `And of t * t
        | `Neg of t
        | `Empty ]

    type tau = 
      [ | `Dyn
        | `TVar of alpha
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
    type alpha_vector = Init_Type.alpha list
    type t_vector = Init_Type.t list

    type p =    (* blame label *)
            | Simple of int
            | Pos of int * int
            | Neg of int * int

    type e = 
      [ | `Var of x
        | `Cst of b
        | `Lam of tau * tau * x * e
        | `App of e * e
        | `Prd of e * e
        | `Pi1 of e
        | `Pi2 of e
        | `Let of x * e * e
        | `TLam of alpha_vector * e
        | `TApp of e * t_vector
        | `TwoCast of e * tau * tau ]

    type v = int 
end

(* Old values *)
(* 
type u = [ `Lam of tau * tau * x * e]
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

        let pprint_p = function 
            | `Simple n -> string_of_int n
            | `Neg (n, m) | `Pos (n, m) -> Printf.sprintf "(%s, %s)" (string_of_int n) (string_of_int m)

        let pprint_alpha_vector = 
            fun av -> "[" ^ (String.concat " ; " av) ^ "]"

        let rec pprint_type  = function
            | `Dyn -> "?"
            | `Bool -> "Bool"
            | `TVar al -> al
            | `Prod (t1, t2) -> Printf.sprintf "(%s, %s)" (pprint_type t1) (pprint_type t2)
            | `Int -> "Int"
            | `Arr (t1, t2) -> Printf.sprintf "%s 🡒  %s" (pprint_type t1) (pprint_type t2)
            | `Or (t1, t2) -> Printf.sprintf "%s ∨ %s" (pprint_type t1) (pprint_type t2)
            | `And (t1, t2) -> Printf.sprintf "%s ∧ %s" (pprint_type t1) (pprint_type t2)
            | `Neg t1 -> Printf.sprintf "¬%s" (pprint_type t1) 
            | `Empty -> "𝟘"

        let pprint_t_vector  = fun tv ->   
            let stv = List.map pprint_type tv in
            String.concat " ; " stv

        let rec pprint_e = function
            | `Var x -> x
            | `Cst b -> begin match b with
                        | `I n -> string_of_int n
                        | `B b -> string_of_bool b end
            | `Lam (t1, t2, x, e) -> 
                Printf.sprintf "λ %s . %s : %s 🡒  %s" x (pprint_e e) (pprint_type t1) (pprint_type t2)
            | `App (e1, e2) -> 
                Printf.sprintf "(%s) %s" (pprint_e e1) (pprint_e e2)
            | `Prd (e1, e2) ->
                Printf.sprintf "(%s, %s)" (pprint_e e1) (pprint_e e2)
            | `Pi1 e -> 
                Printf.sprintf "π_1 %s" (pprint_e e)
            | `Pi2 e ->
                Printf.sprintf "π_1 %s" (pprint_e e)
            | `Let (x, e1, e2) ->
                Printf.sprintf "let %s = %s in %s" x (pprint_e e1) (pprint_e e2)
            | `TLam (av, e) ->
                Printf.sprintf "Λ %s . %s" (pprint_alpha_vector av) (pprint_e e)
            | `TApp (e, tv) ->
                Printf.sprintf "(%s) [%s]" (pprint_e e) (pprint_t_vector tv)
            | `Cast (e, tau1, p, tau2) ->    
                let s_format : _ format =
                    begin match e with
                    (* | `Lam _ ->     "(%s) 〈%s ==[%s]==> %s 〉" *)
                    | _ ->          "%s 〈%s ==[%s]==> %s 〉" end
                in Printf.sprintf s_format (pprint_e e) (pprint_type tau1) (pprint_p p) (pprint_type tau2) 
            | `TwoCast (e, tau1, tau2) ->    
                let s_format : _ format = 
                    (match e with
                    (* | `Lam _ -> "(%s) 〈%s, %s 〉" *)
                    (* | `TwoCast _ -> "%s〈%s, %s 〉" *)
                    | _ -> "%s 〈%s, %s 〉") in
                Printf.sprintf s_format (pprint_e e) (pprint_type tau1) (pprint_type tau2)

        let print_e = 
            fun e -> print_string (pprint_e e)

        let typeof = function
            | `Cst (`I _) -> `Int
            | `Cst (`B _) -> `Bool 
            | `Lam (tau1, tau2, _, _) -> `Arr (tau1, tau2)
            | `TwoCast (`Lam (tau1, tau2, _, _), tau_i, _) -> `And (`Arr (tau1, tau2), tau_i)
            | _ -> failwith "Not a value"
end

(* module type Test = sig
    val match_variant : [< `Ah of string ] -> string
end

module TTest : Test = struct
    let match_variant = function
        | `Ah x -> x
        | `Oh -> ""
end *)

module POPL19 = (Make_Cast_Language(POPL19_Types))(Make_POPL19)
module SE = (Make_Cast_Language(SE_Types))(Make_SE)