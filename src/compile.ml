open Syntax
open Print
open Primitives

module Bytecode1 = struct
    include SE_CDuce

    type interface = 
        | Pass
        | Result of tau
        | Strict of tau * tau

    (* TODO: check if this is correct *)
    (* the question is : what does Strict mean at all steps ? *)
    let comp : interface * tau -> interface = function
    | Pass, t -> Result t
    | Result t, t' -> Result (cap t t')
    | Strict (t1, t2), t' -> Strict (cap t1 t', cap t2 (dom t'))

    type rec_flag = bool

    type byte = 
              | ACC of var
              | CST of b
              | CLS of rec_flag * var * byte list * interface
              | APP                     (* app *)
              | TAP                     (* tailapp *)
              | CAS                     (* cast *)
              | TCA of tau              (* tailcast *)
              | RET
              | SUC   | PRE   | MUL | ADD | SUB
              | TYP of tau
              | LET of var
              | END of var
              | EQB
              | IFZ of byte list * byte list
              | UNI
    (* [@@deriving eq,show] *)

    let show_interface = function 
    | Pass -> "*"
    | Result t -> 
        Printf.sprintf "<%s>"
        (pp_tau t)
    | Strict (tres, tdom) ->
        Printf.sprintf "<%s, %s>" 
        (pp_tau tres) (pp_tau tdom)
    
    let rec show_byte = function
        | UNI ->   "UNIT"
        | ACC v -> "ACC " ^ (pp_var v)
        | CST b -> "CST " ^ (pp_b b)
        | TYP t -> "TYP " ^ (pp_tau t)
        | CAS ->   "CAS"
        | TAP ->   "TAILAPP"
        | TCA t -> 
            Printf.sprintf "TAILCAST %s" (pp_tau t)
        | CLS (frec, v, btc, inter) ->
            let srec = if frec then "Rec" else "" in
            Printf.sprintf "%sCLS (%s, %s, %s)" srec
            (pp_var v) (show_bytecode btc)
            (show_interface inter)
        | APP ->   "APP"
        | RET ->   "RET"
        | SUC ->   "SUC" | MUL -> "MUL" | ADD -> "ADD" | SUB -> "SUB"
        | PRE ->   "PRE"
        | LET v -> "LET " ^ (pp_var v)
        | END v -> "END " ^ (pp_var v)
        | EQB ->   "EQB"
        | IFZ (btc1, btc2) ->
            Printf.sprintf "IFZ (%s , %s)"
            (show_bytecode btc1) (show_bytecode btc2)

    and show_bytecode btc = 
        "[" ^ String.concat " ; " 
        (List.map show_byte btc) ^ "]"

    type bytecode = byte list
    (* [@@deriving eq] *)

end

module Compile1 = struct
    open Bytecode1

    let rec compile : e -> bytecode = function
        | Var x ->                [ACC x]
        | Cst c ->                [CST c]
        | App (e1, e2) ->         (compile e1) @ (compile e2) @ [APP]
        | Lam (t, x, e) ->        [CLS (false, x, (compile e) @ [RET], Strict (t, dom t))]
        | Lamrec (t, x, e) ->     [CLS (true, x, (compile e) @ [RET], Strict (t, dom t))]
        | Cast (App (e1, e2), (tau_1,_)) -> 
                                  (compile e1) @ (compile e2) @ [TCA tau_1]
        | Cast (e, (tau_1, _)) -> [TYP tau_1] @ (compile e) @ [CAS]
        | Succ e ->               (compile e) @ [SUC]
        | Pred e ->               (compile e) @ [PRE]
        | Mult (e1, e2) ->        (compile e1) @ (compile e2) @ [MUL]
        | Plus (e1, e2) ->        (compile e1) @ (compile e2) @ [ADD]
        | Minus (e1, e2) ->        (compile e1) @ (compile e2) @ [SUB]
        | Let (x, e1, e2) ->      (compile e1) @ [LET x] @ (compile e2) @ [END x]
        | Ifz (cond, e1, e2) ->   (compile cond) @ [IFZ (compile e1, compile e2)]
        | Eq (e1, e2) ->          (compile e1) @ (compile e2) @ [EQB]
        | Unit ->                 [UNI]

    and tail_compile : e -> bytecode = function
        | _ -> []
end