open Syntax
open Primitives
open Types
open Types.Print

module Bytecode_Eval_Apply = struct
    include Eager

    type mark = 
        | Static
        | Result
        | Strict

    type kappa = tau * tau

    (* TODO: check if this is correct *)
    (* question : what does (t1,t2) mean at all steps ? *)
    let comp : kappa * tau -> kappa = 
    fun ((t1,t2),t) -> (cap t1 t, cap t2 (dom t))


    type byte = 
              | ACC of var
              | CST of b
              | CLS of var * byte list * kappa
              | RCL of var * var * byte list * kappa
              | TYP of kappa
              | APP                     (* app *)
              | TAP                     (* tailapp *)
              | CAS                     (* kappa *)
              | TCA of kappa              (* tailcast *)
              | RET
              | SUC | PRE | MUL | ADD | SUB
              | LET of var
              | END of var
              | EQB
              | IFZ of byte list * byte list
              | UNI
    (* [@@deriving eq,show] *)

    type bytecode = byte list
    (* [@@deriving eq] *)
end


module Print = struct 
    open Bytecode_Eval_Apply

    let show_mark = function 
        | Static -> "*"
        | Result -> "R"
        | Strict -> "S"

    let show_kappa (t1, t2) = 
        Printf.sprintf "<%s, %s>" (pp_tau t1) (pp_tau t2)
    
    let rec show_byte verb = function
        | UNI ->   "UNIT"
        | ACC v -> "ACC " ^ (pp_var v)
        | CST b -> "CST " ^ (pp_b b)
        | TYP k -> "TYP " ^ (show_kappa k)
        | CAS ->   "CAS"
        | TAP ->   "TAILAPP"
        | TCA k -> 
            Printf.sprintf "TAILCAST %s" (show_kappa k)
        | CLS (v, btc, (t,_)) ->
            begin match verb with
            | 0 -> "CLS"
            | 1 -> 
            Printf.sprintf "CLS (%s,...)" (pp_var v)
            | _ -> 
            Printf.sprintf "CLS (%s, %s, %s)"
            (pp_var v) (show_bytecode verb btc)
            (pp_tau t)  end
        | RCL (f, v, btc, (t, _)) ->
            begin match verb with
            | 0 -> Printf.sprintf "CLS_%s" (pp_var f)
            | 1 -> Printf.sprintf "CLS_%s (%s,...)" (pp_var f) (pp_var v)
            | _ -> Printf.sprintf "CLS_%s (%s, %s, %s)" (pp_var f)
                (pp_var v) (show_bytecode verb btc)
                (pp_tau t)  end
        | APP ->   "APP"
        | RET ->   "RET"
        | SUC ->   "SUC" | MUL -> "MUL" | ADD -> "ADD" | SUB -> "SUB"
        | PRE ->   "PRE"
        | LET v -> "LET " ^ (pp_var v)
        | END v -> "END " ^ (pp_var v)
        | EQB ->   "EQB"
        | IFZ (btc1, btc2) ->
            begin match verb with
            | 0 -> "IFZ"
            | _ ->
            Printf.sprintf "IFZ (%s , %s)"
            (show_bytecode verb btc1) (show_bytecode verb btc2) end

    and show_bytecode verb btc = 
        "[ " ^ String.concat " ; " 
        (List.map (show_byte verb) btc) ^ " ]"
end