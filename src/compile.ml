open Primitives
open Bytecode

module Compile_Eval_Apply = struct
    open Bytecode_Eval_Apply

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
        | Lam (t, x, e) ->                   [CLS (x, (tail_compile e), (t, dom t), Static)]
        | Lamrec (f, t, x, e) ->             [RCL (f, x, (tail_compile e), (t, dom t), Static)]
        | Cast (e, (tau_1, _)) ->            [TYP tau_1] @ (compile e) @ [CAS]
        | Succ e ->                          (compile e) @ [SUC]
        | Pred e ->                          (compile e) @ [PRE]
        | Let (x, e1, e2) ->                 (compile e1) @ [LET x] @ (compile e2) @ [END x]
        | Ifz (cond, e1, e2) ->              (compile cond) @ [IFZ (compile e1, compile e2)]
        | Mult  (e1,e2) | Plus (e1,e2)       
        | Minus (e1,e2) | Eq (e1,e2) as e -> (compile e1) @ (compile e2) @ [get_cons e]
        | Unit ->                            [UNI]
        

    and tail_compile : e -> bytecode = function
        | Cast (App (e1,e2), (tau_1,_)) -> (compile e1) @ (compile e2) @ [TCA tau_1]
        | Ifz (cond, e1, e2) ->            (compile cond) @ [IFZ (tail_compile e1, tail_compile e2)]
        | Let (x, a, b) ->                 (compile a) @ [LET x] @ (tail_compile b)
        | App (e1, e2) ->                  (compile e1) @ (compile e2) @ [TAP]
        | e ->                             (compile e) @ [RET]
end
