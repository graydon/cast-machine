open Syntax
open Types
open Types.Print
open Primitives
open Syntax.CDuce_Gradual_Types


module type Cast_Representation = sig
    type kappa
    val show_kappa : kappa -> string
    val eval : kappa -> tau * tau
    val eval_1 : kappa -> tau
    val eval_2 : kappa -> tau
    val mk_kappa : tau * tau -> kappa
    val mk_dom : kappa -> kappa
    val mk_app : kappa -> tau -> kappa
    val compose : kappa -> kappa -> kappa
end

module Pair : Cast_Representation = struct
    type kappa = tau * tau
    let show_kappa (t1, t2) = 
        Printf.sprintf "<%s, %s>" (pp_tau t1) (pp_tau t2)
    let eval = fun x -> x
    let eval_1 = fun (t,_) -> t
    let eval_2 = fun (_,t) -> t
    let mk_kappa = fun x -> x
    let mk_dom (_,t2) = (t2,dom t2)
    let mk_app (t1,_) t = let tr = result t1 t in (tr,dom tr) 
    let compose k1 k2 =let (t1,t2) = eval k1 in
        let (t3,t4) = eval k2 in mk_kappa ((cap t1 t3), (cap t2 t4))
end

module Symbol : Cast_Representation = struct

    type kappa = Pair of tau * tau 
        | Dom of kappa 
        | App of kappa * tau

    let rec eval_1 = function
        | Pair (t1, _) -> t1
        | Dom k -> eval_2 k
        | App (k,t) -> let t1 = eval_1 k in result t1 t

    and eval_2 = function
        | Pair (_, t2) -> t2
        | Dom k -> dom (eval_2 k)
        | App (k,t) -> let t1 = eval_1 k in dom (result t1 t)

    let rec eval = function
        | Pair (t1, t2) -> (t1, t2)
        | Dom k -> let t2 = eval_2 k in (t2, dom t2)
        | App (k,t) -> let t1 = eval_1 k in
            let tr = result t1 t in (tr, dom tr)

    let show_kappa = fun k -> 
        let (t1, t2) = eval k in 
        Printf.sprintf "<%s, %s>" (pp_tau t1) (pp_tau t2)

    let mk_kappa (t1,t2) = Pair (t1,t2)
    let mk_dom k = Dom k 
    let mk_app k t = App (k,t)
    let compose k1 k2 = let (t1,t2) = eval k1 in
        let (t3,t4) = eval k2 in mk_kappa ((cap t1 t3), (cap t2 t4))
end


module Symbol_Cap : Cast_Representation = struct

    type kappa = Pair of tau * tau 
        | Dom of kappa 
        | App of kappa * tau
        | Cap of kappa * kappa

    let rec eval_1 = function
        | Pair (t1, _) -> t1
        | Dom k -> eval_2 k
        | App (k,t) -> let t1 = eval_1 k in result t1 t
        | Cap (k1,k2) -> let t1 = eval_1 k1 in
            let t2 = eval_1 k2 in cap t1 t2

    and eval_2 = function
        | Pair (_, t2) -> t2
        | Dom k -> dom (eval_2 k)
        | App (k,t) -> let t1 = eval_1 k in dom (result t1 t)
        | Cap (k1,k2) -> let t1' = eval_2 k1 in
            let t2' = eval_2 k2 in cap t1' t2'

    let rec eval = function
        | Pair (t1, t2) -> (t1, t2)
        | Dom k -> let t2 = eval_2 k in (t2, dom t2)
        | App (k,t) -> let t1 = eval_1 k in
            let tr = result t1 t in (tr, dom tr)
        | Cap (k1,k2) -> let (t1,t1') = eval k1 in
            let (t2,t2') = eval k2 in (cap t1 t2, cap t1' t2')

    let show_kappa = fun k -> 
        let (t1, t2) = eval k in 
        Printf.sprintf "<%s, %s>" (pp_tau t1) (pp_tau t2)

    let mk_kappa (t1,t2) = Pair (t1,t2)
    let mk_dom k = Dom k 
    let mk_app k t = App (k,t)
    let compose k1 k2 = Cap (k1,k2)
end

module type Bytecode = sig
    include Cast_Representation

    type mark = Static | Strict
    
    type byte = 
            | ACC of int
            | CST of b
            | CLS of byte list * kappa
            | ACLS of int * bytecode * kappa
            (* | RCL of var * var * byte list * kappa *)
            | TYP of kappa
            | APP                     (* app *)
            | TAP                     (* tailapp *)
            | CAS                     (* kappa *)
            | TCA of kappa              (* tailcast *)
            | RET
            | SUC | PRE | MUL | ADD | SUB | MOD
            | LET
            | END
            | EQB
            | IFZ of byte list * byte list
            | UNI
            | MKP | FST | SND
            (* | LER of var letrec *)

    and bytecode = byte list

        val show_mark : mark -> string
        val show_byte : int -> byte -> string
        val show_bytecode : int -> bytecode -> string
        val show_kappa : kappa -> string
end

module Make_Bytecode (M : Cast_Representation) : Bytecode = struct
    include Eager
    include M

    type mark = 
        | Static
        | Strict

    type byte = 
              | ACC of int 
              | CST of b
              | CLS of byte list * kappa
              | ACLS of int * bytecode * kappa
              (* | RCL of var * var * byte list * kappa *)
              | TYP of kappa
              | APP                     (* app *)
              | TAP                     (* tailapp *)
              | CAS                     (* kappa *)
              | TCA of kappa              (* tailcast *)
              | RET
              | SUC | PRE | MUL | ADD | SUB | MOD
              | LET
              | END
              | EQB
              | IFZ of byte list * byte list
              | UNI
              | MKP | FST | SND
              (* | LER of var letrec *)

    and bytecode = byte list

    (* module Print = struct  *)
        let show_mark = function 
            | Static -> "*"
            | Strict -> "□"
        let rec show_byte verb = function
            (* | LER v -> "LER " ^ (pp_var v)   *)
            | UNI ->   "UNIT" | ACC n -> "ACC " ^ (string_of_int n)
            | CST b -> "CST " ^ (pp_b b) | TYP k -> "TYP " ^ (show_kappa k)
            | FST ->   "FST" | SND   -> "SND" | CAS ->   "CAS" | TAP ->   "TAILAPP"
            | MKP ->   "make_pair" | TCA k ->  
                Printf.sprintf "TAILCAST %s" (show_kappa k)
            | CLS (btc, k) ->
                begin match verb with
                | 0 -> "CLS"
                | 1 -> 
                Printf.sprintf "CLS (...)" 
                | _ -> 
                Printf.sprintf "CLS (%s, %s)"
                (show_bytecode verb btc)
                (show_kappa k)  end
            (* | RCL (f, v, btc, k) ->
                begin match verb with
                | 0 -> Printf.sprintf "CLS_%s" (pp_var f)
                | 1 -> Printf.sprintf "CLS_%s (%s,...)" (pp_var f) (pp_var v)
                | _ -> Printf.sprintf "CLS_%s (%s, %s, %s)" (pp_var f)
                    (pp_var v) (show_bytecode verb btc)
                    (show_kappa k)  end *)
            | APP ->   "APP"
            | RET ->   "RET"
            | SUC ->   "SUC" | MUL -> "MUL" | ADD -> "ADD" | SUB -> "SUB"
            | PRE ->   "PRE"
            | LET -> "LET" 
            | END -> "END"
            | EQB ->   "EQB"
            | MOD -> "MOD"
            | IFZ (btc1, btc2) ->
                begin match verb with
                | 0 -> "IFZ"
                | _ ->
                Printf.sprintf "IFZ (%s , %s)"
                (show_bytecode verb btc1) (show_bytecode verb btc2) end

        and show_bytecode verb btc = 
            "[ " ^ String.concat " ; " 
            (List.map (show_byte verb) btc) ^ " ]"
    (* end *)
end

module Bytecode_Eager = Make_Bytecode(Pair)
module Bytecode_Symbolic = Make_Bytecode(Symbol)
module Bytecode_Symbolic_Cap = Make_Bytecode(Symbol_Cap)


(* module Print = struct 
    open Bytecode_Eager

    let show_mark = function 
        | Static -> "*"
        | Strict -> "□"

    let rec show_byte verb = function
        | LER v -> "LER " ^ (pp_var v) 
        | UNI ->   "UNIT"
        | ACC v -> "ACC " ^ (pp_var v)
        | CST b -> "CST " ^ (pp_b b)
        | TYP k -> "TYP " ^ (show_kappa k)
        | FST ->   "FST"
        | SND   -> "SND"
        | CAS ->   "CAS"
        | TAP ->   "TAILAPP"
        | MKP ->   "make_pair"
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
        | LET -> "LET " ^ (pp_var v)
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
end *)