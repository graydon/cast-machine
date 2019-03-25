(* Interpreter written using big step semantics *)

open Primitives
open Syntax
open SE_CDuce
open Print

module Eager_Calculus = struct

    module Env = Map.Make(
        struct 
            type t = var
            let compare = Pervasives.compare
        end
    )

    type twosome =
        | I of tau * tau
        | T of tau * tau

    type v = 
            [ | `Cst of b 
            | `Closure of (tau * tau * var * e) * twosome * env
            | `Fail
            ]
    and env = v Env.t

    let inter ts (t1,t2) = match ts with
        | T (t1', t2') | I (t1', t2') -> T (cap t1 t1', cap t2 t2')
    (* let check_ceil c tau = SE_CDuce.subtype  *)

    let (typeof : v -> t) = function
        | `Cst b -> constant b
        | `Closure (_, (I (t1', t2') | T (t1', t2')), _) -> 
            arrow (cons t1') (cons t2')
        | `Fail -> failwith "error: type of `Fail"

    let eval (e : SE_CDuce.e) = 
        let rec aux env = function
            | Var x -> Env.find x env
            | Cst b -> `Cst b
            | Lam (tau1, tau2, x, e) -> `Closure ((tau1, tau2, x, e), I (tau1, tau2), Env.empty)
            | TwoCast (e, tau1, tau2) -> 
                begin match (aux env e) with
                | `Cst c -> if subtype (constant c) (ceil tau1) then `Cst c else `Fail
                | `Closure (e', tau, env') ->
                    `Closure (e', inter tau (tau1, tau2), env') 
                | `Fail -> `Fail end
            | App (e1, e2) ->
                begin match (aux env e1) with
                | `Cst _ -> failwith "error: trying to apply a constant"
                | `Fail -> `Fail
                | `Closure (((_, _, x, e') , (I (tau1, tau2) | T (tau1, tau2)), env')) -> 
                    let v' = aux env e2 in
                    let v0 = aux env (TwoCast (e2, tau2, dom tau2)) in
                    let env'' = Env.add x v0 env' in
                    begin match (aux env'' e') with
                    | `Cst c -> if subtype (constant c) (ceil (apply tau1 (typeof v'))) then `Cst c else `Fail
                    | `Closure (fe'', tau', env'') ->
                        let tapp = apply tau1 (typeof v') in
                        `Closure (fe'', inter tau' (tapp, dom tapp), env'')
                    | `Fail -> `Fail (* trying to apply `Fail as a function *)
                    end
                end
            | Cast (e', tau) -> `Fail
        in aux Env.empty e 
        
    let wrap_eval e = 
        Printf.printf "code : %s\n" (pprint_e e);
        print_string  "eval : ";
        begin match eval e with
        | `Fail -> print_string "Fail"
        | `Cst b -> print_string (pp_const Format.str_formatter b; Format.flush_str_formatter ())
        | `Closure ((t1, t2, x, e), _, _) -> print_e (Lam (t1, t2, x, e)) end; 
        print_endline "\n"
end

module Lazy_Calculus = struct
    module Env = Map.Make(
            struct 
                type t = var
                let compare = Pervasives.compare
            end
        )

    type sigma =
        | Id of sigma 
        | C of t
        | Comp of sigma * sigma
        | App of t * sigma
        | Dom of sigma

    type funcast = t * t

    let rec eval_sigma : sigma -> funcast = function
        | C t -> (t, dom t)
        | Id s -> eval_sigma s
        | Comp (s1, s2) -> 
            let (t1, t2), (t1', t2') = eval_sigma s1, eval_sigma s2 in
            (cap t1 t1', cap t2 t2')
        | App (t, s) -> 
            let (t1, t2) = eval_sigma s in
            let t' = apply t1 t in (t', dom t')
        | Dom s -> 
            let (_, t2) = eval_sigma s in
            (t2, dom t2)

    type v = 
            [ | `Cst of b 
            | `Closure of (tau * tau * var * e) * sigma * env
            | `Fail
            ]
    and env = v Env.t
end 