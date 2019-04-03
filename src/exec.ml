open Compile
open Primitives
open Print

module Exec1 = struct
    include Compile1
    open Bytecode1

    module Env = Map.Make(struct 
        type t = var
        let compare = Pervasives.compare end)

    type result = [
        | `CST of b
        | `Fail ]

    type stack_value = 
        [ `CST of b 
        | `BTC of bytecode 
        | `ENV of env
        | `CLS of var * bytecode ]
    and env = stack_value Env.t

    type stack = stack_value list

    type dump = env list (*wrong; todo*)

    type state = bytecode * env * stack * dump

    
    let rec show_stack_value = function
    | `CST b -> pp_b b
    | `BTC btc -> show_bytecode btc
    | `ENV env -> show_env env
    | `CLS (v, btc) -> 
        Printf.sprintf "C(%s, %s)"
        (pp_var v) (show_bytecode btc)

    and show_env : env -> string =
    fun env ->
        let lenv = List.of_seq (Env.to_seq env) in
        String.concat " , "
            (List.map
                (fun (v,sv) -> Printf.sprintf "(%s := %s)"
                (pp_var v) (show_stack_value sv)) 
                lenv)

    let print_stack s =
        Printf.sprintf "[ %s ]" 
        (String.concat " . "
        (List.map show_stack_value s))

    exception Machine_Stack_Overflow

    type run_params =
        {run : bool ref;
         step : int ref;
         max_stack : int ref;
         verbose : int ref}

    let run_params =
        {run = ref true;
         step = ref 0;
         max_stack = ref 300;
         verbose = ref 1}

    let print_debug_stack run_params s =
        let stack_size = List.length s in
        if stack_size < 20 && !(run_params.verbose) >= 1
        then Printf.printf "Stack[%i]: %s\n" (stack_size) (print_stack s)
        else Printf.printf "Stack[%i]\n" (stack_size) 

    let print_debug_code run_params s =
        let stack_size = List.length s in
        if stack_size < 20 && !(run_params.verbose) >= 1
        then Printf.printf "Code [%i]: %s\n" (stack_size) (show_bytecode s)
        else Printf.printf "Code [%i]\n" (stack_size) 

    let print_debug_env run_params s =
        let stack_size = List.length (List.of_seq @@ Env.to_seq s) in
        if stack_size < 20 && !(run_params.verbose) >= 1
        then Printf.printf "Env  [%i]: %s\n" (stack_size) (show_env s)
        else Printf.printf "Env  [%i]\n" (stack_size) 
    

    let print_debug_run run_params = function
        | c, e, s, _ -> 
        Printf.printf "==={%i}===\n" !(run_params.step); incr (run_params.step);
        print_debug_code run_params c;
        print_debug_stack run_params s;
        print_debug_env run_params e
        
    let run_check run_params (_, _, s, _) =
        if List.length s > !(run_params.max_stack)
        then raise Machine_Stack_Overflow

    let run code env = 
        let rec aux : state -> state = fun state ->
        print_debug_run run_params state;
        run_check run_params state;
        match state with
            | CST b :: c, e, s, d -> 
                aux (c, e, `CST b :: s, d)

            | ACC x :: c, e, s, d ->
                aux (c, e, (Env.find x e) :: s, d)

            | CLS (x, c') :: c, e, s, d ->
                aux (c, e, `CLS (x,c') :: s, d)

            | APP :: c, e, `CLS (x,c') :: v :: s, d ->
                let e' = Env.add x v e in
                aux (c', e', `BTC c :: `ENV e :: s, d)

            | RET :: _, _, v :: `BTC c' :: `ENV e' :: s, d ->
                aux (c', e', v :: s, d)

            | SUC :: c, e, `CST (Integer i) :: s, d ->
                aux (c, e, `CST (Integer (succ i)) :: s, d)

            | PRE :: c, e, `CST (Integer i) :: s, d ->
                aux (c, e, `CST (Integer (pred i)) :: s, d)

            | LET x :: c, e, v :: s, d ->
                let e' = Env.add x v e in
                aux (c, e', s, e :: d)
            
            | END :: c, e, s, e' :: d ->
                aux (c, e', s, d)

            | s -> s

        in aux (code, env, [], [])
    
    let run_init code =
        run code Env.empty

    let finish = function 
        | _, _, v :: _, _ -> v
        | _ -> failwith "no return value"

    let print_value : stack_value -> unit = function
        | `CST c -> Print.print_e (Cst c)
        | `BTC _ | `ENV _ -> failwith "wrong type of return value on stack"
        | `CLS (x,_) -> Printf.printf "fun %s -> code" (Print.pp_var x)

    let wrap_run code = 
        let v = finish (run_init code) in
        print_string "- " ; print_value v; print_endline ""
end