(* TODO put env in closures *)
(* init closures with a mark * *)
(* note : Beppe sait comment avoir les types intersections *)

open Compile
open Primitives
open Print

module Exec1 = struct
    include Compile1
    open Bytecode1

    module Env = Hashtbl.Make(struct 
        type t = var
        let equal = (=)
        let hash = Hashtbl.hash
        end)

    type result = [
        | `CST of b
        | `Fail ]

    type stack_value = 
        [ `CST of b 
        | `BTC of bytecode 
        | `ENV of env
        | `CLS of var * bytecode * env * interface
        | `TYP of tau
        | `FAIL
        ]
    and env = stack_value Env.t

    type stack = stack_value list

    type dump_item = 
        | Boundary of tau
        | Frame of bytecode * env
    type dump = dump_item list 

    type state = bytecode * env * stack * dump


    let typeof_stack_value : stack_value -> t = function
    | `CST b -> constant b
    | `CLS (v, btc, env, (Strict (t, _) | Result t)) -> t
    | _ -> failwith "error: trying to take typeof of `ENV or bytecode"

    let rec show_stack_value : stack_value -> string = function
    | `CST b -> pp_b b
    | `BTC btc -> show_bytecode btc
    | `ENV env -> show_env env
    | `CLS (v, btc, env, inter) -> 
        Printf.sprintf "C(%s, %s, %s, %s)"
        (pp_var v) (show_bytecode btc) (show_env env)
        (show_interface inter)
    | `TYP t -> pp_tau t
    | `FAIL -> "Fail"

    and show_env_value : stack_value -> string = function
    | `CST b -> pp_b b
    | `BTC btc -> show_bytecode btc
    | `ENV env -> "{ nonempty env }"
    | `CLS (v, btc, env, inter) -> 
        Printf.sprintf "C(%s, %s, { env }, %s)"
        (pp_var v) (show_bytecode btc)  (show_interface inter)
    | `TYP t -> pp_tau t
    | `FAIL -> "Fail"

    and show_result : stack_value -> string = function
    | `CST b -> 
        Printf.sprintf ": %s = %s" (pp_tau (constant b)) (pp_b b)
    | `CLS (v, btc, _, _) -> 
        Printf.sprintf ": %s -> %s = <fun>" (pp_var v) (show_bytecode btc)
    | `FAIL -> "Fail"
    | _ -> failwith "not a return value"

    and show_env : env -> string =
    fun env ->
        let lenv = List.of_seq (Env.to_seq env) in
        "{" ^ String.concat " , "
            (List.map
                (fun (v,sv) -> Printf.sprintf "(%s := %s)"
                (pp_var v) (show_env_value sv)) 
                lenv) ^ "}"

    and show_dump_item : dump_item -> string = function
    | Boundary t -> Printf.sprintf "<%s>" (pp_tau t)
    | Frame (c,e) -> Printf.sprintf "([code], {env})"

    and show_dump : dump -> string =
    fun d ->
        (String.concat " . "
        (List.map show_dump_item d))

    let show_stack_value_1 : stack_value -> string = function
    | `CST b -> pp_b b
    | `BTC _ -> "[code]"
    | `ENV _ -> "{env}"
    | `CLS _ -> "C(...)"
    | `TYP t -> pp_tau t
    | `FAIL -> "Fail" 

    let print_stack s verbose = 
    let show_stack = begin match verbose with
    | 2 -> show_stack_value
    | 1 -> show_stack_value_1
    | 0 -> fun _ -> ""
    | _ -> failwith "wrong verbose argument" end
    in
        Printf.sprintf "[ %s ]" 
        (String.concat " . "
        (List.map show_stack s))

    exception Machine_Stack_Overflow

    type run_params =
        {run : bool ref;
         step : int ref;
         max_stack : int ref;
         verbose : int ref;
         delim : int ref;
         debug : bool ref}

    let run_params =
        {run = ref true;
         step = ref 0;
         max_stack = ref 300;
         verbose = ref 2;
         delim = ref 2;
         debug = ref true}

    let delim n i =
        let si = string_of_int i in 
        let d = String.length si in
        String.init (n+1-d) (fun _ -> ' ')

    let print_debug_stack run_params s =
        let stack_size = List.length s in
        if stack_size < 10 && !(run_params.verbose) >= 1
        then 
        let d = delim !(run_params.delim) stack_size in
        let ssize = string_of_int stack_size in 
        let strstack = print_stack s !(run_params.verbose) in
        Printf.printf "Stack[%s]:%s %s\n" ssize d strstack
        else 
         Printf.printf "Stack[%i]\n" (stack_size) 

    let print_debug_code run_params s =
        let stack_size = List.length s in
        if stack_size < 20 && !(run_params.verbose) >= 1
        then Printf.printf "Code [%i]:%s%s\n" (stack_size) 
        (delim !(run_params.delim) stack_size) (show_bytecode s)
        else Printf.printf "Code [%i]\n" (stack_size) 

    let print_debug_env run_params s =
        let stack_size = List.length (List.of_seq @@ Env.to_seq s) in
        if stack_size < 20 && !(run_params.verbose) >= 1
        then Printf.printf "Env  [%i]:%s%s\n" (stack_size)
        (delim !(run_params.delim) stack_size) (show_env s)
        else Printf.printf "Env  [%i]\n" (stack_size) 

    let print_debug_dump run_params s =
        let stack_size = List.length s in
        if stack_size < 20 && !(run_params.verbose) >= 1
        then Printf.printf "Env  [%i]:%s%s\n" (stack_size)
        (delim !(run_params.delim) stack_size) (show_dump s)
        else Printf.printf "Env  [%i]\n" (stack_size) 
    
    let print_debug_run run_params = function
        | c, e, s, d -> 
        Printf.printf "==={%i}========================================================================\n" !(run_params.step); incr (run_params.step);
        print_debug_code run_params c;
        print_debug_stack run_params s;
        print_debug_env run_params e;
        print_debug_dump run_params d;
        Pervasives.flush stdout
        
    let run_check run_params (_, _, s, _) =
        if List.length s > !(run_params.max_stack)
        then raise Machine_Stack_Overflow

    let run code env = 
        let rec aux : state -> state = fun state ->
        let () = if !(run_params.debug) then
        print_debug_run run_params state in
        run_check run_params state;
        match state with
            | CST b :: c, e, s, d -> 
                aux (c, e, `CST b :: s, d)

            | ACC x :: c, e, s, d ->
                aux (c, e, (Env.find e x ) :: s, d)

            | IFZ (c1, _) :: c, e, `CST b :: s, d 
                when b = Primitives.zero ->
                aux (c1 @ c, e, s, d)

            | IFZ (_, c2) :: c, e, _ :: s, d ->
                aux (c2 @ c, e, s, d)

            | CLS (false, x, c', inter) :: c, e, s, d ->
                aux (c, e, `CLS (x, c', Env.copy e, inter) :: s, d)

            | CLS (true, x, c', inter) :: c, e, s, d ->
                let () = Env.add e x (`CLS (x, c', Env.copy e, inter)) in
                aux (c, e, `CLS (x, c', Env.copy e, inter) :: s, d)
            

            | APP :: c, e,  v :: `CLS (x, c', e', Pass) :: s, d ->
                let () = Env.add e' x v in
                aux (c', e', `BTC c :: `ENV e :: s, Frame (c, Env.copy e) :: d)

            | RET :: _, _, v :: s, Frame (c', e') :: d ->
                aux (c', e', v :: s, d)

            | SUC :: c, e, `CST (Integer i) :: s, d ->
                aux (c, e, `CST (Integer (succ i)) :: s, d)

            | PRE :: c, e, `CST (Integer i) :: s, d ->
                aux (c, e, `CST (Integer (pred i)) :: s, d)

            | MUL :: c, e, `CST (Integer i1) :: `CST (Integer i2) :: s, d ->
                aux (c, e, `CST (Integer (mult i1 i2)) :: s, d)

            | ADD :: c, e, `CST (Integer i1) :: `CST (Integer i2) :: s, d ->
                aux (c, e, `CST (Integer (add i1 i2)) :: s, d) 

            | LET x :: c, e, v :: s, d ->
                let e' = Env.copy e in
                let () = Env.add e' x v  in
                aux (c, e', s, d)
            
            | END x :: c, e, s, d ->
                let () = Env.remove e x in
                aux (c, e, s, d)

            (* new instructions for casts *)

            | (TAP|APP) :: c, e,  `FAIL :: _ :: s, d -> 
                aux (c, e, `FAIL :: s, d)
            
            | (TAP|APP) :: c, e, _ :: `FAIL :: s, d -> 
                aux (c, e, `FAIL :: s, d)

            | TCA t :: c, e, v :: `CLS (x, c', e', Pass) :: s, Frame (c'', e'') :: d ->
                let () = Env.add e' x v in
                aux (c', e', s, Boundary t :: d)

            | TCA t :: c, e, v :: `CLS (x, c', e', Pass) :: s, Boundary t' :: d ->
                let () = Env.add e' x v in
                aux (c', e', s, Boundary (cap t t') :: d)

            | APP :: c, e,  v :: `CLS (x, c', e', Result t) :: s, d ->
                let () = Env.add e' x v in
                let tres = apply t (typeof_stack_value v) in
                aux (c', e', `TYP tres :: s, Frame (CAS :: c, e) :: d)

            | APP :: c, e,  v :: `CLS (x, c', e', Strict (tres, tdom)) :: s, d ->
                let () = Env.add e' x v in
                aux (CAS :: APP :: c, e, 
                     v :: `TYP tdom :: `CLS (x, c', e', Result tres) :: s, 
                     Frame (c, e) :: d)

            | TAP :: c, e,  v :: `CLS (x, c', e', Result t) :: s, d ->
                aux (TCA t :: c, e, v :: `CLS (x, c', e', Pass) :: s, d)

            | TAP :: c, e,  v :: `CLS (x, c', e', Strict (tres, tdom)) :: s, d ->
                aux (CAS :: TAP :: c, e, v :: `TYP tdom :: `CLS (x, c', e', Result tres) :: s, d)

            | TAP :: c, e,  v :: `CLS (x, c', e', Pass) :: s, d ->
                let () = Env.add e' x v in
                aux (c', e', `BTC c :: `ENV e :: s, d)

            | CAS :: c, e, `CST b :: `TYP t :: s, d 
                when subtype (constant b) (ceil t) ->
                aux (c, e, `CST b :: s, d)

            | CAS :: c, e, `CST b :: `TYP t :: s, d ->
                aux (c, e, `FAIL :: s, d)

            | CAS :: c, e, `CLS (x, c', e', t') :: `TYP t :: s, d ->
                let t'' = comp (t', t) in
                aux (c, e, `CLS (x, c', e', t'') :: s, d)

            | TYP t :: c, e, s, d ->
                aux (c, e, `TYP t :: s, d)

            | s -> s

        in aux (code, env, [], [])
    
    let run_init code =
        let () = if !(run_params.debug) then print_endline "Run initialization" in
        run code @@ Env.create 20

    let finish = function 
        | _, _, [v], _ -> v
        | _, _, _ :: _ :: _, _ -> failwith "unfinished computation"
        | _ -> failwith "no return value"

    (* let print_value : stack_value -> unit = function
        | `CST c -> Print.print_e (Cst c)
        | `BTC _ | `ENV _ -> failwith "wrong type of return value on stack"
        | `CLS (x,_,_,_) -> Printf.printf "fun %s -> code and env" (Print.pp_var x)
        | `TYP t -> pp_tau t *)

    let wrap_run code debug verbose = 
        let () = run_params.debug := debug in
        let () = run_params.verbose := verbose in
        let v = finish (run_init code) in
        print_string "- " ; print_string @@ show_result v; print_endline ""
end