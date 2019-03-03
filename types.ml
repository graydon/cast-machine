type alpha = string

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

(* Cast expressions *)

type x = string
type b = [ `I of int | `B of bool ]
type alpha_vector = alpha list
type t_vector = t list

type p =    (* blame label *)
    | Simple of int
    | Pos of int * int
    | Neg of int * int

type e = 
    | Var of x
    | Cst of b
    | Lam of tau * tau * x * e
    | App of e * e
    | Prd of e * e
    | Pi1 of e
    | Pi2 of e
    | Let of x * e * e
    | TLam of alpha_vector * e
    | TApp of e * t_vector
    | Cast of e * tau * p * tau


let pprint_p : p -> string = function 
    | Simple n -> string_of_int n
    | Neg (n, m) | Pos (n, m) -> Printf.sprintf "(%s, %s)" (string_of_int n) (string_of_int m)

let pprint_alpha_vector : alpha_vector -> string = 
    fun av -> "[" ^ (String.concat " ; " av) ^ "]"

let rec pprint_t : t -> string = function
    | `Bool -> "Bool"
    | `TVar al -> al
    | `Prod (t1, t2) -> Printf.sprintf "(%s, %s)" (pprint_t t1) (pprint_t t2)
    | `Int -> "Int"
    | `Arr (t1, t2) -> Printf.sprintf "%s 🡒 %s" (pprint_t t1) (pprint_t t2)

let rec pprint_tau : tau -> string = function
    | `Dyn -> "?"
    | `Bool -> "Bool"
    | `TVar al -> al
    | `Prod (t1, t2) -> Printf.sprintf "(%s, %s)" (pprint_tau t1) (pprint_tau t2)
    | `Int -> "Int"
    | `Arr (t1, t2) -> Printf.sprintf "%s 🡒 %s" (pprint_tau t1) (pprint_tau t2)

let pprint_t_vector : t_vector -> string = fun tv ->   
    let stv = List.map pprint_t tv in
    String.concat " ; " stv

let rec pprint_e : e -> string = function
    | Var x -> x
    | Cst b -> (match b with
                | `I n -> string_of_int n
                | `B b -> string_of_bool b)
    | Lam (t1, t2, x, e) -> 
        Printf.sprintf "λ %s . %s : %s 🡒 %s" x (pprint_e e) (pprint_tau t1) (pprint_tau t2)
    | App (e1, e2) -> 
        Printf.sprintf "(%s) %s" (pprint_e e1) (pprint_e e2)
    | Prd (e1, e2) ->
        Printf.sprintf "(%s, %s)" (pprint_e e1) (pprint_e e2)
    | Pi1 e -> 
        Printf.sprintf "π_1 %s" (pprint_e e)
    | Pi2 e ->
        Printf.sprintf "π_1 %s" (pprint_e e)
    | Let (x, e1, e2) ->
        Printf.sprintf "let %s = %s in %s" x (pprint_e e1) (pprint_e e2)
    | TLam (av, e) ->
        Printf.sprintf "Λ %s . %s" (pprint_alpha_vector av) (pprint_e e)
    | TApp (e, tv) ->
        Printf.sprintf "(%s) [%s]" (pprint_e e) (pprint_t_vector tv)
    | Cast (e, tau1, p, tau2) ->    
        match e with
        | Lam _ -> Printf.sprintf "(%s) 〈%s ==[%s]==> %s〉" (pprint_e e) (pprint_tau tau1) (pprint_p p) (pprint_tau tau2) 
        | _ -> Printf.sprintf "%s 〈%s ==[%s]==> %s〉" (pprint_e e) (pprint_tau tau1)  (pprint_p p) (pprint_tau tau2)

let print_e : e -> unit = 
    fun e -> print_string (pprint_e e)






