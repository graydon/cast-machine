open Bytecode
open Types
open Primitives

module Make_Compile (B : Bytecode) = struct
    open Syntax.Eager
    open B

    let get_cons : e -> byte = function
        | App _ ->   APP
        | Succ _ ->  SUC
        | Pred _ ->  PRE
        | Mult _ ->  MUL
        | Plus _ ->  ADD
        | Minus _ -> SUB
        | Eq _ ->    EQB
        | _ -> failwith "get_cons misuse"

    let rec compile : e -> bytecode = function
        | Var x ->                           [ACC x]
        | Cst c ->                           [CST c]
        | App (e1, e2) ->                    (compile e1) @ (compile e2) @ [APP]
        | Fst e ->                           (compile e) @ [FST]
        | Snd e ->                           (compile e) @ [SND]
        | Lam (t, x, e) ->                   [CLS (x, (tail_compile e), mk_kappa (t, dom t))]
        | Cast (e, k) ->                     [TYP (mk_kappa k)] @ (compile e) @ [CAS]
        | Pair (e1, e2) ->                   (compile e1) @ (compile e2) @ [MKP]
        | Succ e ->                          (compile e) @ [SUC]
        | Pred e ->                          (compile e) @ [PRE]
        | Let (x, e1, e2) ->                 (compile e1) @ [LET x] @ (compile e2) @ [END x]
        | Letrec (f, e1, e2) ->              (rec_compile f e1) @ [LET f] @ (compile e2) @ [END f]
        | Ifz (cond, e1, e2) ->              (compile cond) @ [IFZ (compile e1, compile e2)]
        | Mult  (e1,e2) | Plus (e1,e2)       
        | Minus (e1,e2) | Eq (e1,e2) as e -> (compile e1) @ (compile e2) @ [get_cons e]
        | Unit ->                            [UNI]
        

    and tail_compile : e -> bytecode = function
        | Cast (App (e1,e2), k) -> (compile e1) @ (compile e2) @ [TCA (mk_kappa k)]
        | Ifz (cond, e1, e2) ->    (compile cond) @ [IFZ (tail_compile e1, tail_compile e2)]
        | Let (x, a, b) ->         (compile a) @ [LET x] @ (tail_compile b)
        | Letrec (x, a, b) ->      (rec_compile x a) @ [LER x] @ (tail_compile b) @ [END x] 
        | App (e1, e2) ->          (compile e1) @ (compile e2) @ [TAP]
        | e ->                     (compile e) @ [RET]

    and rec_compile : var -> e -> bytecode = fun f -> function
        | Lam (t, x, e) ->      [RCL (f, x, tail_compile e, mk_kappa (t, dom t))]
        | Let (x, e1, e2) ->    (compile e1) @ [LET x] @ (rec_compile f e2) @ [END x]
        | Letrec (g, e1, e2) -> fun_compile f (g,e1) e2
        (* wrong *)
        (* | Pair ((Lam (t1, _, _) as l1), (Lam (t2, _, _) as l2)) -> 
            let xf = fresh_var () in
            let pt = pair t1 t2 in
            let c1 = compile l1 in
            let c2 = compile l2 in 
            [RCL (f, xf, (ACC xf :: c1) @ (ACC xf :: c2) @ [MKP], (pt, dom pt))] *)
        | e ->                  (compile e)

    and fun_compile : var -> var * e -> e -> bytecode = fun f (g,e1) -> function
        | Lam (t, x , e) -> [RCL (f, x, tail_compile (Letrec (g, e1, e)), mk_kappa (t,dom t))]
        | e ->              compile e (*WRONG - TODO*)
        (* | Let (x, e1', e2) ->        fun_compile f ( *) (* TODO *)

end

module Compile = Make_Compile(Bytecode_Eager)
module Compile_Symbolic = Make_Compile(Bytecode_Symbolic)
