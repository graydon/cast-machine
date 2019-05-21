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

    let transform : e -> e = function
        | e -> e

    let extend_rho xs r = List.fold_left (fun r1 x -> mk_rho r1 x) r xs

    let rec compile r : e -> bytecode = function
        | Var x ->                      [ACC (lookup x r)]
        | Cst c ->                      [CST c]
        | App (e1, e2) ->               (compile r e1) @ (compile r e2) @ [APP]
        | Apply (n, t, f, e, xs, es) -> let r1 = extend_rho xs r in let r2 = mk_rho r1 f in
                                        [ACLS (n, tail_compile r2 e, mk_kappa (t,dom t))]
        | Fst e ->                      (compile r e) @ [FST]
        | Snd e ->                      (compile r e) @ [SND]
        | Mu (t, f, x, e) ->            [CLS (tail_compile (mk_rho (mk_rho r f) x) e, mk_kappa (t, dom t))]
        | Cast (e, k) ->                [TYP (mk_kappa k)] @ (compile r e) @ [CAS]
        | Pair (e1, e2) ->              (compile r e1) @ (compile r e2) @ [MKP]
        | Succ e ->                     (compile r e) @ [SUC]
        | Pred e ->                     (compile r e) @ [PRE]
        | Let (x, e1, e2) ->            (compile r e1) @ [LET] @ (compile (mk_rho r x) e2) @ [END]
        | Letrec _ ->                   failwith "letrec should all be eliminated at compile"
        | Ifz (cond, e1, e2) ->         (compile r cond) @ [IFZ (compile r e1, compile r e2)]
        | Mult (e1,e2) | Plus (e1,e2) 
        | Mod (e1,e2) | Minus (e1,e2) 
        | Eq (e1,e2) as e ->           (compile r e1) @ (compile r e2) @ [get_cons e]
        | LetP ((x, y), e1, e2) -> 
            let p = Primitives.fresh_var () in 
            compile r @@ Let (p, e1, Let (x, Fst (Var p), Let (y, Snd (Var p), e2)))
        

    and tail_compile r : e -> bytecode = function
        | Cast (App (e1,e2), k) -> (compile r e1) @ (compile r e2) @ [TCA (mk_kappa k)]
        | Ifz (cond, e1, e2) ->    (compile r cond) @ [IFZ (tail_compile r e1, tail_compile r e2)]
        | Let (x, a, b) ->         (compile r a) @ [LET] @ (tail_compile (mk_rho r x) b)
        | App (e1, e2) ->          (compile r e1) @ (compile r e2) @ [TAP]
        | LetP ((x, y), e1, e2) -> 
            let p = Primitives.fresh_var () in 
            tail_compile r @@ Let (p, e1, Let (x, Fst (Var p), Let (y, Snd (Var p), e2)))
        | Letrec _ -> failwith "no letrec"
        | e ->                     (compile r e) @ [RET]

end

module Compile = Make_Compile(Bytecode_Eager)
module Compile_Symbolic = Make_Compile(Bytecode_Symbolic)
