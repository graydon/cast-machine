(* Interpreter written using big step semantics *)

open Casts
open SE_CDuce

module Env = Map.Make(
    struct 
        type t = var
        let compare = Pervasives.compare
    end
)

type v = 
        [ | `Cst of b 
          | `Closure of [ `Lam of tau * tau * var * e ] * (tau * tau) * env
          | `Fail
          ]
and env = v Env.t

(* let check_ceil c tau = SE_CDuce.subtype  *)

let eval e = 
    let aux = fun e env ->
        match e with
        | `Var x -> Env.find x env
        | `Cst b -> `Cst b
        (* | `TwoCast (e', tau1, tau2) ->
            begin match aux e' env with
            | ` *)
        (* | `Closure (e, t, env) -> `Closure (e, t, env)
        | `Lam (tau1, tau2, _, _) -> `Closure (e, (tau1, tau2), env) (* wrong *)
        | `TwoCast (e', tau1, tau2) -> 
            let v = aux e' env in 
            begin match v with
                | `Closure (e', (tau1', tau2'), env') 
                    -> `Closure (e', (CD.Types.cap tau1 tau1', CD.Types.cap tau2 tau2'), env')
                | `Cst c 
                    -> failwith "not implemented"
            end
        | `App (e1, e2) -> 
            let e1' = aux e1 env in
            begin match e1' with
                | `Closure (`Lam (_, _, x, e'), (tau1, tau2), env') -> 
                    let v' = aux e2 env in
                    begin match v' with
                    | `Cst c -> 
                    | `Closure (e', t', env') ->
                    let v0 = aux (`TwoCast (v', tau2, tau2)) env in
                    let env' = Env.add x v0 env in
                        aux e' env'
                | _ -> failwith "error: constant applied as a function"
            end
        | _ -> failwith "error: not implemented" *)
    in aux e Env.empty  