type alpha = string

(** POPL19 types and cast expressions *)

type t = [
    | `TVar of alpha
    | `Int 
    | `Bool
    | `Prod of t * t 
    | `Arr of t * t 
    ]

type tau = [ 
    | `Dyn
    | `TVar of alpha
    | `Int 
    | `Bool
    | `Prod of tau * tau
    | `Arr of tau * tau
    ]

type x = string
type b = [ `I of int | `B of bool ]
type alpha_vector = alpha list
type t_vector = t list

type p =    (* blame label *)
    | Simple of int
    | Pos of int * int
    | Neg of int * int

type e = [
    | `Var of x
    | `Cst of b
    | `Lam of tau * tau * x * e
    | `App of e * e
    | `Prd of e * e
    | `Pi1 of e
    | `Pi2 of e
    | `Let of x * e * e
    | `TLam of alpha_vector * e
    | `TApp of e * t_vector
    | `Cast of e * tau * p * tau
    ]


(* Victor's types and expressions from "Space-efficient [...]" notes *)

type t' = [
    | `TVar of alpha
    | `Int 
    | `Bool
    | `Arr of t' * t'
    | `Or of t' * t'
    | `And of t' * t'
    | `Neg of t'
    | `Empty
    ]

type tau' = [ 
    | `Dyn
    | `TVar of alpha
    | `Int 
    | `Bool
    | `Arr of tau' * tau'
    | `Or of tau' * tau'
    | `And of tau' * tau'
    | `Neg of tau'
    | `Empty
    ]

type e' = [
    | `Var of x
    | `Cst of b
    | `Lam of tau' * tau' * x * e'
    | `App of e' * e'
    | `Prd of e' * e'
    | `Pi1 of e'
    | `Pi2 of e'
    | `Let of x * e' * e'
    | `TLam of alpha_vector * e'
    | `TApp of e' * t_vector
    | `TwoCast of e' * tau' * tau'
    ]

(* Boolean functions to determine the value among these terms *)
type u = [ `Lam of tau' * tau' * x * e']
type value = [ u | `Cst of b | `TwoCast of u * tau' * tau' ]

let is_u : e' -> bool = function
    | `Lam _ | `Cst _ -> true
    | _ -> false

let is_v : e' -> bool = fun e ->
    if is_u e then true 
    else match e with
        | `TwoCast (`Lam _, _, _) -> true
        | _ -> false


(* Printing functions *)

let pprint_p : p -> string = function 
    | Simple n -> string_of_int n
    | Neg (n, m) | Pos (n, m) -> Printf.sprintf "(%s, %s)" (string_of_int n) (string_of_int m)

let pprint_alpha_vector : alpha_vector -> string = 
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

let pprint_t_vector : t_vector -> string = fun tv ->   
    let stv = List.map pprint_type tv in
    String.concat " ; " stv

let rec pprint_e = function
    | `Var x -> x
    | `Cst b -> (match b with
                | `I n -> string_of_int n
                | `B b -> string_of_bool b)
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
            (match e with
            | `Lam _ ->     "(%s) 〈%s ==[%s]==> %s〉"
            | _ ->          "%s 〈%s ==[%s]==> %s〉")
        in Printf.sprintf s_format (pprint_e e) (pprint_type tau1) (pprint_p p) (pprint_type tau2) 
        
    | `TwoCast (e, tau1, tau2) ->    
        let s_format : _ format = 
            (match e with
            | `Lam _ -> "(%s) 〈%s, %s〉"
            | `TwoCast _ -> "%s〈%s, %s〉"
            | _ -> "%s 〈%s, %s〉") in
        Printf.sprintf s_format (pprint_e e) (pprint_type tau1) (pprint_type tau2)


let print_e = 
    fun e -> print_string (pprint_e e)


let typeof = function
    | `Cst (`I _) -> `Int
    | `Cst (`B _) -> `Bool 
    | `Lam (tau1, tau2, _, _) -> `Arr (tau1, tau2)
    | _ -> failwith "Not a value"