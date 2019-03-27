(* Interpreter written using big step semantics *)
open Primitives
open Syntax

(* Working interpreter - this is the more eager version
of the space-efficient semantics *)
module Eager_Calculus = struct
    include SE_CDuce
    open Print.Print

    module Env = Map.Make(struct 
            type t = var
            let compare = Pervasives.compare end)
            
    type betared = S | T (* simple or typed *)
    type twosome = tau * tau * betared
    type v = 
        [ | `Cst of b 
        | `Closure of (tau * tau * var * e) * twosome * env
        | `Fail
        ]
    and env = v Env.t

    let inter ts (t1,t2) = match ts with
        | (t1', t2', _)  -> (cap t1 t1', cap t2 t2', T)
        
    let typeof : v -> t = function
        | `Cst b -> constant b
        | `Closure (_, (t1', t2', _), _) -> 
            arrow (cons t1') (cons t2')
        | `Fail -> failwith "error: type of `Fail"

    let eval : e -> v = fun e ->
        let rec aux : env -> e -> v = fun env -> function
        | Var x -> begin
            try Env.find x env
            with Not_found -> 
            failwith (Printf.sprintf "error: variable %s not found" (CD.Var.ident x))
            end
        | Succ e -> begin match aux env e with
            | `Cst (Integer i) -> `Cst (Integer (CD.Intervals.V.succ i))
            | _ -> `Fail end
        | Pred e -> begin match aux env e with
            | `Cst (Integer i) -> `Cst (Integer (CD.Intervals.V.pred i))
            | _ -> `Fail end
        | Cst b -> `Cst b
        | Lam (tau1, tau2, x, e) -> 
            `Closure ((tau1, tau2, x, e), (tau1, tau2, T), Env.empty)
        | Cast (e, (tau1, tau2)) -> 
            begin match (aux env e) with
            | `Cst c -> if subtype (constant c) (ceil tau1) then `Cst c else `Fail
            | `Closure (e', tau, env') ->
                `Closure (e', inter tau (tau1, tau2), env') 
            | `Fail -> `Fail end
        | App (e1, e2) ->
            begin match (aux env e1) with
            | `Cst _ -> failwith "error: trying to apply a constant"
            | `Fail -> `Fail
            | `Closure (((_, _, x, e') , (tau1, tau2, _), env')) -> 
                let v' = aux env e2 in
                let v0 = aux env (Cast (e2, (tau2, dom tau2))) in
                let env'' = Env.add x v0 env' in
                begin match (aux env'' e') with
                | `Cst c -> if subtype (constant c) (ceil (apply tau1 (typeof v'))) then `Cst c else `Fail
                | `Closure (fe'', tau', _) ->
                    let tapp = apply tau1 (typeof v') in
                    `Closure (fe'', inter tau' (tapp, dom tapp), env'')
                | `Fail -> `Fail (* trying to apply `Fail as a function *)
                end
            end
        in aux Env.empty e 
        
    let wrap_eval e =
    (* try  *)
        Printf.printf "code : %s\n" (pprint_e e);
        print_string  "eval : ";
        begin match eval e with
        | `Fail -> print_string "Fail"
        | `Cst b -> print_string (pp_const Format.str_formatter b; Format.flush_str_formatter ())
        | `Closure ((t1, t2, x, e), _, _) -> print_e (Lam (t1, t2, x, e)) end; 
        print_endline "\n"
    (* with Stack_overflow ->
        print_endline "error: OCaml stack overflow\n" *)
end

(* Symbolic version (todo) *)
module Symbolic_Calculus = struct
    include SE_CDuce_Symbolic
    open Print.Print_Symbolic

    module Env = Map.Make(
            struct 
                type t = var
                let compare = Pervasives.compare
            end
        )
    let rec eval_sigma : sigma -> tau * tau = function
        | Cast t -> (t, dom t)
        | Id t -> (t, dom t)
        | Comp (s1, s2) -> 
            let (t1, t2), (t1', t2') = eval_sigma s1, eval_sigma s2 in
            (cap t1 t1', cap t2 t2')
        | App (t, s) -> 
            let (t1, t2) = eval_sigma s in
            let t' = apply t1 t in (t', dom t')
        | Dom s -> 
            let (_, t2) = eval_sigma s in
            (t2, dom t2)

    let eval1 sigma = let (t1, _) = eval_sigma sigma in t1

    type v = 
        [ | `Cst of b 
        | `Closure of (tau * tau * var * e) * sigma * env
        | `Fail
        ]
    and venv = [ `Cast of v * sigma ]
    and env = venv Env.t
        
    let typeof : v -> t = function
        | `Cst b -> constant b
        | `Closure (_, sigma, _) -> 
            let (t1', t2') = eval_sigma sigma in
            arrow (cons t1') (cons t2')
        | `Fail -> failwith "error: trying to take typeof of `Fail"

    let rec eval_venv : env -> venv -> v = fun env (`Cast (v, s)) -> 
        match v with
        | `Cst b -> eval_aux env (Cast (Cst b, s))
        | `Closure (e', s', env') -> `Closure (e', comp s s', env')
        | `Fail -> `Fail

    and eval_aux : env -> e -> v = fun env -> function
    | Var x -> eval_venv env (Env.find x env)
    | Succ e -> begin match eval_aux env e with
        | `Cst (Integer i) -> `Cst (Integer (CD.Intervals.V.succ i))
        | _ -> `Fail end
    | Pred e -> begin match eval_aux env e with
        | `Cst (Integer i) -> `Cst (Integer (CD.Intervals.V.pred i))
        | _ -> `Fail end
    | Cst b -> `Cst b
    | Lam (tau1, tau2, x, e) -> 
        `Closure ((tau1, tau2, x, e), Id (mk_arrow tau1 tau2), Env.empty)
    | Cast (e, sigma1) -> 
        begin match (eval_aux env e) with
        | `Cst c -> if subtype (constant c) (ceil (eval1 sigma1)) then `Cst c else `Fail
        | `Closure (e', sigma2, env') ->
            `Closure (e', comp sigma1 sigma2, env') 
        | `Fail -> `Fail end
    | App (e1, e2) ->
        begin match (eval_aux env e1) with
        | `Cst _ -> failwith "error: trying to apply a constant"
        | `Fail -> `Fail
        | `Closure (((_, _, x, e') , sigma1, env')) -> 
            let v' = eval_aux env e2 in
            (* here is the distinction lazy/eager for this symbolic calc *)
            (* let v0 = eval_aux env (Cast (e2, Dom sigma1)) in *)
            let env'' = Env.add x (`Cast (v', Dom sigma1)) env' in
            begin match (eval_aux env'' e') with
            | `Cst c -> let tau1 = eval1 sigma1 in 
                if subtype (constant c) (ceil (apply tau1 (typeof v'))) 
                then `Cst c else `Fail
            | `Closure (fe'', sigma2, env'') ->
                `Closure (fe'', Comp (App (typeof v', sigma1), sigma2), env'')
            | `Fail -> `Fail (* trying to apply `Fail as a function *)
            end
        end
    
    let eval : e -> v = fun e -> eval_aux Env.empty e 

    let wrap_eval : e -> unit = fun e ->
        Printf.printf "code : %s\n" (pprint_e e);
        print_string  "eval : ";
        begin match eval e with
        | `Fail -> print_string "Fail"
        | `Cst b -> print_string (pp_const Format.str_formatter b; Format.flush_str_formatter ())
        | `Closure ((t1, t2, x, e), _, _) -> print_e (Lam (t1, t2, x, e)) end; 
        print_endline "\n"
end 


(* Lazier version without symbolic operation (todo) *)
module Lazy_Calculus = struct
end