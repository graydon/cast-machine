open Bytecode
open Types

module Make_Compile (B : Bytecode) = struct
    open Syntax.Eager
    open B

    type rho = Nil | Var of var * rho

    let mk_rho r v = Var (v, r)

    let rec lookup x = function
        | Nil -> failwith "error[rho]: not found"
        | Var (y, _) when x = y -> 0
        | Var (_, r) -> 1 + lookup x r

    let get_cons : e -> byte = function
        | App _ ->   APP
        | Succ _ ->  SUC
        | Pred _ ->  PRE
        | Mult _ ->  MUL
        | Plus _ ->  ADD
        | Mod _ -> MOD
        | Minus _ -> SUB
        | Eq _ ->    EQB
        | _ -> failwith "get_cons misuse"

    let dummy () = Primitives.fresh_var ()

    let rec compile r : e -> bytecode = function
        | Var x ->                     [ACC (lookup x r)]
        | Cst c ->                     [CST c]
        | App (e1, e2) ->              (compile r e1) @ (compile r e2) @ [APP]
        | Fst e ->                     (compile r e) @ [FST]
        | Snd e ->                     (compile r e) @ [SND]
        | Mu (t, f, x, e) ->           [CLS (tail_compile (mk_rho (mk_rho r x) f) e, mk_kappa (t, dom t))]
        | Cast (e, k) ->               [TYP (mk_kappa k)] @ (compile r e) @ [CAS]
        | Pair (e1, e2) ->             (compile r e1) @ (compile r e2) @ [MKP]
        | Succ e ->                    (compile r e) @ [SUC]
        | Pred e ->                    (compile r e) @ [PRE]
        | Let (x, e1, e2) ->           (compile r e1) @ [LET] @ (compile (mk_rho r x) e2) @ [END]
        | Letrec _ ->       failwith "letrec should all be eliminated at compile"
        | Ifz (cond, e1, e2) ->        (compile r cond) @ [IFZ (compile r e1, compile r e2)]
        | Mult (e1,e2) | Plus (e1,e2) 
        | Mod (e1,e2) | Minus (e1,e2) 
        | Eq (e1,e2) as e ->           (compile r e1) @ (compile r e2) @ [get_cons e]
        

    and tail_compile r : e -> bytecode = function
        | Cast (App (e1,e2), k) -> (compile r e1) @ (compile r e2) @ [TCA (mk_kappa k)]
        | Ifz (cond, e1, e2) ->    (compile r cond) @ [IFZ (tail_compile r e1, tail_compile r e2)]
        | Let (x, a, b) ->         (compile r a) @ [LET] @ (tail_compile (mk_rho r x) b)
        (* | Letrec (x, a, b) ->      (rec_compile r x a) @ [LER x] @ (tail_compile r b) @ [END]  *)
        | App (e1, e2) ->          (compile r e1) @ (compile r e2) @ [TAP]
        | e ->                     (compile r e) @ [RET]

    (* and rec_compile r : var -> e -> bytecode = fun f -> function
        | Lam (t, x, e) ->      [RCL (f, x, tail_compile r e, mk_kappa (t, dom t))]
        | Let (x, e1, e2) ->    (compile r e1) @ [LET] @ (rec_compile r f e2) @ [END]
        | Letrec (g, e1, e2) -> fun_compile r f (g,e1) e2 *)
        (* wrong *)
        (* | Pair ((Lam (t1, _, _) as l1), (Lam (t2, _, _) as l2)) -> 
            let xf = fresh_var () in
            let pt = pair t1 t2 in
            let c1 = compile r l1 in
            let c2 = compile r l2 in 
            [RCL (f, xf, (ACC xf :: c1) @ (ACC xf :: c2) @ [MKP], (pt, dom pt))] *)
        (* | e ->                  (compile r e) *)

    (* and fun_compile r : var -> var * e -> e -> bytecode = fun f (g,e1) -> function
        | Lam (t, x , e) -> [RCL (f, x, tail_compile r (Letrec (g, e1, e)), mk_kappa (t,dom t))]
        | e ->              compile r e WRONG - TODO *)
        (* | Let (x, e1', e2) ->        fun_compile f ( *) (* TODO *)

end

module Compile = Make_Compile(Bytecode_Eager)
module Compile_Symbolic = Make_Compile(Bytecode_Symbolic)
