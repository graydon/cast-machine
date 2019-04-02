open Primitives
open Syntax

module Bytecode1 = struct
    include SE_CDuce

    type byte = ACC of var
              | CST of b
              | CLS of var * bytecode
              | APP
              | RET
              | SUC
              | PRE
              | CAS of bytecode * tau
    and bytecode = byte list 
end

module Compile1 = struct
    open Bytecode1

    let rec compile : e -> bytecode = function
        | Var x ->                [ACC x]
        | Cst c ->                [CST c]
        | App (e1, e2) ->         (compile e1) @ (compile e2) @ [APP]
        | Lam (_, x, e) ->     [CLS (x, (compile e) @ [RET])]
        | Cast (e, (tau_1, _)) -> [CAS (compile e, tau_1)]
        | Succ e ->               (compile e) @ [SUC]
        | Pred e ->               (compile e) @ [PRE]
end