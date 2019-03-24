(** Useless functor; but gives the beginning of current Print modules *)
module Make_PPrint = functor () -> struct
    let pprint_var = fun var -> var

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
        | `Var var -> pprint_var var
        | `Cst b -> begin match b with
                    | `I n -> string_of_int n
                    | `B b -> string_of_bool b end
        | `Lam (t1, t2, var, e) -> 
            Printf.sprintf "λ %s . %s : %s 🡒  %s" (pprint_var var) (pprint_e e) (pprint_type t1) (pprint_type t2)
        | `App (e1, e2) -> 
            Printf.sprintf "(%s) %s" (pprint_e e1) (pprint_e e2)
        | `Prd (e1, e2) ->
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
            Printf.sprintf "(%s) [%s]" (pprint_e e) (pprint_t_vector tv)
        | `Cast (e, tau1, p, tau2) ->    
            let s_format : _ format =
                begin match e with
                | `Lam _ ->     "(%s) 〈%s ==[%s]==> %s 〉" (* careful: influences the variant type *)
                | _ ->          "%s 〈%s ==[%s]==> %s 〉" end
            in Printf.sprintf s_format (pprint_e e) (pprint_type tau1) (pprint_p p) (pprint_type tau2) 
        | `TwoCast (e, tau1, tau2) ->    
            let s_format : _ format = 
                (match e with
                | `Lam _ -> "(%s) 〈%s, %s 〉" (* careful: influences the variant type *)
                | `TwoCast _ -> "%s〈%s, %s 〉" (* careful: influences the variant type *)
                | _ -> "%s 〈%s, %s 〉") in
            Printf.sprintf s_format (pprint_e e) (pprint_type tau1) (pprint_type tau2)

    let print_e = 
        fun e -> print_string (pprint_e e)
end

module PPrint = Make_PPrint()
include PPrint