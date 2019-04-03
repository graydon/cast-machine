open Syntax
open Print

module Bytecode1 = struct
    include SE_CDuce


    type byte = 
              | ACC of var
              | CST of b
              | CLS of var * (byte list)
              | APP
              | RET
              | SUC
              | PRE
              | CAS of (byte list) * tau
              | LET of var
              | END
    (* [@@deriving eq,show] *)

    let rec show_byte = function
        | ACC v -> "ACC " ^ (pp_var v)
        | CST b -> "CST" ^ (pp_b b)
        | CLS (v, btc) ->
            Printf.sprintf "CLS (%s , %s)"
            (pp_var v) (show_bytecode btc)
        | APP ->   "APP"
        | RET -> "RET"
        | SUC ->"SUC"
        | PRE -> "PRE"
        | CAS (btc,tau) ->
            Printf.sprintf "CAS (%s)<%s>"
            (show_bytecode btc) (pp_tau tau)
        | LET v -> "LET " ^ (pp_var v)
        | END -> "END"
    and show_bytecode btc = 
        if btc = [] then "empty" else
        String.concat " ; " 
        (List.map show_byte btc)

    type bytecode = byte list
    (* [@@deriving eq] *)
    
    
end

module Compile1 = struct
    open Bytecode1

    let rec compile : e -> bytecode = function
        | Var x ->                [ACC x]
        | Cst c ->                [CST c]
        | App (e1, e2) ->         (compile e1) @ (compile e2) @ [APP]
        | Lam (_, x, e) ->        [CLS (x, (compile e) @ [RET])]
        | Cast (e, (tau_1, _)) -> [CAS (compile e, tau_1)]
        | Succ e ->               (compile e) @ [SUC]
        | Pred e ->               (compile e) @ [PRE]
        | Let (x, e1, e2) ->      (compile e1) @ [LET x] @ (compile e2) @ [END]

end