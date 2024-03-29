open Primitives
open Utils
open Bytecode
open Types
open Types.Print
open Bytecode_Eager
open Syntax.Eager

(* #define MONITOR "1" *)
(* #define BENCH "1" *)
(* #define DEBUG "1" *)

(* module Env = struct
    include Hashtbl.Make(struct
        `type `t = var
        let equal = (=)
        let hash = Hashtbl.hash
    end)
end *)

module type Structures = sig
    type result
    type stack_item
    type stack
    type dump
    type env
    type state = bytecode * env * stack * dump
    type nu
    val empty_env : env
    val typeof : stack_item -> tau
end

module Make_Machine (B : Bytecode) = struct
    open B

    type result = [
        | `CST of b
        | `Fail ]

    and stack_item =
        [ `CST of b
        | `FCST of (b -> b)
        | `CLS of bytecode * env * kappa * mark
        | `ACLS of int * bytecode * env * kappa * mark
        | `TYP of kappa
        | `ARR of b array
        | `FAIL
        | `PAIR of stack_item * stack_item
        ]
    and env = stack_item list
    type stack = stack_item list

    let access e n = List.nth e n

    let empty_env : env = []

    (* machine values *)
    (* type nu = [
        | `CST of b
        | `CLS of var * bytecode * env * kappa * mark
        | `PAIR of nu * nu ] *)

    type dump_item =
        | Boundary of kappa
        | Frame    of bytecode * env
    type dump = dump_item list

    type state = bytecode * env * stack * dump

    let n_t_dyn = neg t_dyn

    let rec typeof : stack_item -> t = function
        | `CST b -> cap (constant b) (t_dyn)
        | `CLS (_, _, k, _) -> eval_1 k
        | `ACLS (_,_,_,k,_) -> eval_1 k
        | `PAIR (v1, v2) -> pair (typeof v1) (typeof v2)
        | _ -> failwith "error: trying to take typeof of `TYP or `FAIL"

      (* parameter functions *)
        (* let compose : kappa -> kappa -> kappa = fun k1 k2 ->
            let (t1,t2) = eval k1 in
            let (t3,t4) = eval k2 in mk_kappa ((cap t1 t3), (cap t2 t4)) *)

        let dump : kappa -> dump -> dump = fun k -> function
        | [] -> [Boundary k]
        | Boundary k' :: d' -> Boundary (compose k k') :: d'
        | (Frame _ :: _) as d -> Boundary k :: d


    let rec applycast : stack_item -> kappa -> stack_item = fun v k ->
        let (t,td) = eval k in
        begin match v with
        | `CST b -> if subtype (constant b) (ceil t) then `CST b
            else `FAIL
        | `PAIR (v1, v2) ->
            let t1, t2 = pi1 t, pi2 t in
            let k1, k2 = mk_kappa (t1,dom t1), mk_kappa (t2,dom t2) in
            let cv1 = applycast v1 k1  in
            let cv2 = applycast v2 k2 in
            begin match cv1, cv2 with
            | `FAIL, _ -> `FAIL
            | _, `FAIL -> `FAIL
            | _ -> `PAIR (cv1, cv2) end
        | `CLS (c,e,k',_) ->
            if td = t_bot then `FAIL
            else let kc = compose k k' in
            `CLS (c,e,kc,Strict)
        | `ACLS (n,c,e,k',_) ->
            if td = t_bot then `FAIL
            else let kc = compose k k' in
            `ACLS (n,c,e,kc,Strict)

        | _ -> failwith "wrong object to be cast" end


    module Print = struct
        let rec show_stack_value : stack_item -> string = function
        | `CST b -> pp_b b
        | `FCST _ -> ""
        | `CLS (btc, env, ts, m) ->
            Printf.sprintf "C(%s, %s, %s, %s)"
            (show_bytecode 2 btc) (show_env 1  env)
            (show_kappa ts) (show_mark m)
        | `ACLS (n,btc, env, ts, m) ->
            Printf.sprintf "C_%i(%s, %s, %s, %s)" n
            (show_bytecode 2 btc) (show_env 1  env)
            (show_kappa ts) (show_mark m)
        | `TYP k -> show_kappa k
        | `FAIL -> "Fail"
        | `PAIR (v1, v2) ->
            Printf.sprintf "(%s, %s)" (show_stack_value v1)
            (show_stack_value v2)
        | `ARR _ ->"[a]"

        and sh_env_v : int -> stack_item -> string =
        function
        | 2 -> show_stack_value
        | 0 -> (fun _ -> "_")
        | 1 -> show_stack_value_1
        | _ -> failwith "wrong verbose argument"

        and show_result : stack_item -> string = function
        | `CST b ->
            Printf.sprintf "%s = %s" (pp_tau (constant b)) (pp_b b)
        | `CLS (_,_, k,_) ->
            let t = eval_1 k in
            Printf.sprintf "%s = <fun>" (pp_tau t)
        | `FAIL -> "Fail"
        | `PAIR (v1, v2) as v ->
            Printf.sprintf "%s = (%s, %s)" (pp_tau @@ typeof v)
            (show_result v1) (show_result v2)
        | `ARR _ -> "[a]" 
        | _ -> failwith "not a return value"

        and show_env verb ?inline:(i=true) : env -> string = fun e ->
            let sep = if i then " . " else "\n\t     " in
            String.concat sep (List.map (fun v -> sh_env_v verb v) e)
        (* fun verb inline env ->
            let lenv = List.of_seq (Env.to_seq env) in
            let sep = if inline then " . " else "\n\t     " in
            if lenv = [] then "{}" else
            "{ " ^ String.concat sep
                (List.map
                    (fun (v,sv) -> Printf.sprintf "(%s := %s)"
                    (pp_var v) (sh_env_v verb sv)) 
                    lenv) ^ " }" *)

        and show_dump_item verb : dump_item -> string = function
        | Boundary t -> Printf.sprintf "<%s>" (show_kappa t)
        | Frame (_,e) -> Printf.sprintf "([code], %s)" (show_env verb  e)

        and show_dump verb : dump -> string =
        fun d ->
            (String.concat "\n\n\t   "
            (List.map (show_dump_item verb) d))

        and show_stack_value_1 : stack_item -> string = function
        | `CST b -> pp_b b
        | `CLS (c,env,bnd,_) -> Printf.sprintf "[c[%i],e[%i],%s]" 
            (List.length c) (List.length env) (show_kappa bnd)
        | `TYP t -> show_kappa t
        | `FAIL -> "Fail" 
        | `PAIR (v1, v2) -> 
            Printf.sprintf "(%s, %s)" (show_stack_value_1 v1)
            (show_stack_value_1 v2)
        | `ARR a -> 
            "[" ^ (String.concat ";" (Array.to_list (
                Array.map (fun v -> pp_b v) a
            ))) ^ "]"
        | _->"not implem"

        let show_stack s verbose = 
        let show_stack_val = begin match verbose with
        | 2 -> show_stack_value
        | 1 -> show_stack_value_1
        | 0 -> fun _ -> ""
        | _ -> failwith "wrong verbose argument" end
        in
            Printf.sprintf "[ %s ]" 
            (String.concat "\n\t     "
            (List.map show_stack_val s))
    end

    module MetricsDebug = struct 
        open Print

        module MetricsEnv = Hashtbl.Make(struct 
            type t = byte
            let equal a b = match a, b with 
            | ACC _, ACC _ | CST _, CST _
            | CLS _, CLS _ 
            | LET , LET  | TYP _, TYP _
            | END , END  | TCA _, TCA _
            | IFZ _, IFZ _ -> true
            | _ -> a = b
            let hash = Hashtbl.hash
            end)

        type metrics = 
            {mutable stack_sizes : (int * int) list;
            mutable longest_proxies : (int * int) list;
            mutable casts : (int * int) list;
            instructions : int MetricsEnv.t;
            mutable dump_sizes : (int * int) list;
            mutable env_sizes : (int * int) list;
            mutable dump_frames : (int * int) list;
            mutable dump_bounds : (int * int) list;
            }


        type run_params =
            {run : bool ref;
            step : int ref;
            max_stack : int ref;
            max_env : int ref;
            verbose : int ref;
            delim : int ref;
            debug : bool ref;
            step_mode : bool ref;
            step_start : int ref;
            monitor : bool ref;
            mutable states : state list;
            mutable metrics : metrics}


        let init_metrics : unit -> metrics = fun () ->
            {stack_sizes = [];  
            longest_proxies = []; 
            casts = [];
            instructions = MetricsEnv.create 20;
            dump_sizes = [];
            env_sizes = [];
            dump_frames = []; dump_bounds = []
            }

        let run_params =
                {run = ref true;
                step = ref 0;
                max_stack = ref 1000;
                max_env = ref 1000;
                verbose = ref 2;
                delim = ref 2;
                debug = ref true;
                step_mode = ref false;
                step_start = ref 0;
                monitor = ref false;
                states = [];
                metrics = init_metrics ()}
        
        let count_cast : stack -> int =
            let rec aux acc = function
            | [] -> acc
            | `TYP _ :: s -> aux (acc+1) s
            | _ :: s -> aux acc s
        in aux 0 

        let count_bound dmp : int = 
            List.fold_left (fun n e -> match e with | Boundary _ -> n+1 | _ -> n) 0 dmp

        let longest_proxy : stack -> int = 
            let rec aux max acc = function
            | [] -> max
            | `TYP _ :: s when acc+1 > max -> 
                aux (acc+1) (acc+1) s
            | `TYP _ :: s -> 
                aux max (acc+1) s
            | _ :: s -> 
                aux max 0 s
        in aux 0 0


        let env_length : env -> int = List.length
        let dump_length d = List.fold_left 
                (fun n -> function | Frame (_,e) -> env_length e + n
                                   | _ -> n) 0 d
        let dump_frame_length d = List.length d



        let gather_metrics : run_params -> state -> unit =
            fun run_params -> let met = run_params.metrics in
            fun (c, e, s, d) ->
            begin
            (* let () = print_endline "coucou" in *)
            met.dump_bounds<-  (!(run_params.step), count_bound d) :: met.dump_bounds;
            met.env_sizes <- (!(run_params.step), List.length e) :: met.env_sizes;
            met.stack_sizes <- (!(run_params.step), (List.length s)) :: met.stack_sizes;
            met.dump_sizes <- (!(run_params.step), dump_length d) :: met.dump_sizes;
            met.dump_frames <- (!(run_params.step), dump_frame_length d) :: met.dump_frames;
            met.longest_proxies <- (!(run_params.step), (longest_proxy s)) :: met.longest_proxies;
            met.casts <- (!(run_params.step), (count_cast s)) :: met.casts;
            run_params.metrics <- met;
            if c != [] then let instr = List.hd c in
            let cnt_inst =
                (try MetricsEnv.find met.instructions instr
                with Not_found -> 0) in
            let () = Printf.printf "counting one instruction %s" (show_byte 1 instr) in
            MetricsEnv.replace met.instructions instr (cnt_inst+1)
            end

        let delim n i =
            let si = string_of_int i in
            let d = String.length si in
            String.init (n+1-d) (fun _ -> ' ')

        let rec firstk k xs = match xs with
        | [] -> []
        | x::xs -> if k=1 then [x] else x::firstk (k-1) xs;;


        let print_debug_stack run_params s =
            let stack_size = List.length s in
            if !(run_params.verbose) >= 1
            then
            let d = delim !(run_params.delim) stack_size in
            let ssize = string_of_int stack_size in
            let strstack = show_stack (firstk 20 s) !(run_params.verbose) in
            Printf.printf "Stack[%s]:%s%s\n" ssize d strstack
            else
            Printf.printf "Stack[%i]\n" (stack_size)

        let print_debug_code run_params s =
            let stack_size = List.length s in
            if !(run_params.verbose) >= 1
            then Printf.printf "Code [%i]:%s%s\n" (stack_size)
            (delim !(run_params.delim) stack_size)
            (show_bytecode !(run_params.verbose) (firstk 7 s))
            else Printf.printf "Code [%i]\n" (stack_size)

        let print_debug_env run_params s =
            let stack_size = env_length s in
            if stack_size < 20 && !(run_params.verbose) >= 1
            then Printf.printf "Env  [%i]:%s%s\n"
            (stack_size)
            (delim !(run_params.delim) stack_size)
            (show_env !(run_params.verbose) ~inline:false s)
            else Printf.printf "Env  [%i]\n" (stack_size)

        let print_debug_dump run_params s =
            let stack_size = dump_length s in
            if !(run_params.verbose) >= 1
            then Printf.printf "Dump [%i][%i][#casts=%i]:\n\t  %s%s\n" (List.length s) (stack_size)
            (count_bound s)     (delim !(run_params.delim) stack_size)
            (show_dump !(run_params.verbose) (firstk 4 s))
            else Printf.printf "Dump [%i][%i][casts=%i]\n" (List.length s) (stack_size)
            (count_bound s)

        let print_debug_run run_params  = function
            | c, e, s, d -> 
            Printf.printf "==={%i}========================================================================\n" !(run_params.step);
            Pervasives.flush stdout; 
            print_debug_code run_params c;
            Pervasives.flush stdout; print_endline "";
            print_debug_stack run_params s;
            Pervasives.flush stdout; print_endline "";
            print_debug_env run_params e;
            Pervasives.flush stdout; print_endline "";
            print_debug_dump run_params d;
            Pervasives.flush stdout
    end

    module Transitions = struct
        open MetricsDebug

        exception Machine_Stack_Overflow of int * int
        exception Dump_Stack_Overflow of int * int
        exception Env_Overflow of int

        let run_check run_params (_, e, s, d) =
            if List.length s > !(run_params.max_stack) 
            then raise (Machine_Stack_Overflow (List.length s, !(run_params.step)))
            else if dump_length d > !(run_params.max_stack) * !(run_params.max_env)
            then raise (Dump_Stack_Overflow (dump_length d, !(run_params.step)))
            else if List.length d > !(run_params.max_stack)
            then raise (Dump_Stack_Overflow (List.length d, !(run_params.step)))
            else if env_length e > !(run_params.max_env)
            then raise (Env_Overflow !(run_params.step))

        let run_procedures state =
        begin
            (* let () = print_endline "coucou" in *)
            run_params.step := !(run_params.step)+1;
            (* if !(run_params.monitor) then gather_metrics run_params state; *)
#ifndef MONITOR
            if !(run_params.debug) then print_debug_run run_params state;
            (* run_check run_params state; *)
#endif
(* #ifdef STEPMODE *)
            let ref_state = ref state in
                if !(run_params.step_mode)
                && !(run_params.step) >= !(run_params.step_start) then
                begin
                    run_params.debug := true;
                    let cmd = read_line () in
                    if cmd = "b" then
                        (begin
                        ref_state := List.hd run_params.states;
                        run_params.states <- List.tl run_params.states;
                        run_params.step := !(run_params.step)-2
                        end)
                    else if cmd = "2" then
                        run_params.verbose := 2
                    else if cmd = "1" then
                        run_params.verbose := 1
                    else if cmd = "0" then
                        run_params.verbose := 0
                    else run_params.states <- state :: run_params.states
                end;
                !ref_state
(* #endif *)
(* #ifndef STEPMODE *)
            (* state *)
(* #endif *)
        end


        (* let cast : v -> kappa   *)

        let run code env =
            let rec aux : state -> state = fun state ->
#ifndef BENCH
            (* let () = print_endline "hello" in *)
            let state = run_procedures state in
#endif
(* #ifdef MONITOR *)
            (* let () = print_endline "coucou" in *)
            (* gather_metrics run_params state; *)
(* #endif *)
            match state with
                | c, `FAIL ::e, s, d ->
                    aux ([],empty_env,`FAIL::s,Frame(c,e)::d)

                | ACC n :: c, e, s, d ->
                    aux (c, e, (access e n) :: s, d)


                | CST b :: c, e, s, d ->
                    aux (c, e, `CST b :: s, d)

                | CLS (c', k) :: c, e, s, d ->
                    let rec cls = `CLS (c',cls::e,k,Static) in
                    aux (c, e, cls::s, d)

                | TYP k :: c, e, s, d ->
                    aux (c, e, `TYP k :: s, d)

                | APP :: c, e,  v :: `CLS (c', e', _, Static)  :: s, d ->
                    (* let () = print_endline "\n=====\nDebug:" in
                    let () = Printf.printf "Dump extended with code: %s" (show_bytecode 1 c) in
                    let () = Printf.printf "and environment e[%i] = %s" (List.length e) (show_env 1  e) in *)
                    aux (c', v :: e', s, Frame (c, e) :: d)

                | APP :: c, e,  v :: `CLS (c', e', k, Strict) :: s, d ->
                    (* let (ta1,ta2) = eval k in
                    let () = print_endline "Debug tau" in
                    let () = print_endline (pp_tau ta1) in
                    let () = print_endline (pp_tau ta2) in 
                    let tv = typeof v in *)
                    (* let () = print_endline "Debug typeof v" in *)
                    (* let () = print_endline (pp_tau tv) in *)
                    let kr = mk_app k (typeof v) in
                    (* let (t1,t2) = eval kr in 
                    let () = print_endline "Debug tau circ bool" in
                    let () = print_endline (pp_tau t1) in
                    let () = print_endline (pp_tau t2) in *)
                    let kd = mk_dom k in
                    let v' = applycast v kd in
                        aux (c', v'::e', `TYP kr::s, Frame(CAS::c,e)::d)

                | APP :: c, e, `CST b :: `FCST o :: s, d ->
                    let b' = o b in 
                    aux (c, e, `CST b' :: s, d)

                | TAP :: _, _,  v :: `CLS (c', e', _, Static) :: s, d ->
                    aux (c', v :: e', s, d)
                
                | TAP :: _, e,  `CST c :: `FCST o :: s, d ->
                    let c' = o c in
                    aux ([RET], e, `CST c' :: s, d)

                | TAP :: _, _,  v :: `CLS (c', e', k, Strict) :: s, d ->
                    let kr = mk_app k (typeof v) in
                    let kd = mk_dom k in
                    let v' = applycast v kd in
                        aux (c', v'::e', s, dump kr d)

                | RET :: c, e, v :: s, Boundary k :: d ->
                    aux (CAS :: RET :: c, e, v :: `TYP k :: s, d)

                | RET :: _, _, v :: s, Frame (c', e') :: d ->
                    aux (c', e', v :: s, d)

                | TCA k::_, _, v::`CLS (c',e',_,Static)::s, d ->
                    aux (c', v::e', s, dump k d)

                | TCA k :: _, _, v :: `CLS (c',e',k',Strict) :: s, d ->
                    let kr = mk_app k' (typeof v) in
                    let kd = mk_dom k in
                    let v' = applycast v kd in
                        aux (c', v'::e', s, dump (compose k kr) d)
                    (* aux (CAS::TCA (compose k kr):: c, e, v::`TYP kd::`CLS (c',e',k',Static) :: s, d) *)

                (* | CAS :: c, e, `CST b :: `TYP k :: s, d ->
                    let t = eval_1 k in
                    if subtype (constant b) (ceil t)
                    then aux (c, e, `CST b :: s, d)
                    else aux ([], empty_env, `FAIL :: s, Frame (c, e) :: d)

                | CAS :: c, e, `CLS (c',e',k,_) :: `TYP k':: s, d ->
                    let t2' = eval_2 k' in
                    if is_bottom t2' then
                    aux ([], empty_env, `FAIL :: s, Frame (c, e) :: d)
                    else let kc = compose k k' in
                    aux (c, e, `CLS (c',e',kc,Strict) :: s, d) *)

                | CAS :: c, e,  v :: `TYP k:: s, d ->
                    let v' = applycast v k in
                    begin match v' with
                    | `FAIL -> aux ([], empty_env, `FAIL :: s, Frame (c,e) :: d)
                    | _ -> aux (c, e, v'::s, d) end

                | LET :: c, e, v :: s, d ->
                    aux (c, v :: e, s, d)

                | END :: c, _ :: e, s, d ->
                    aux (c, e, s, d)

                | MKP :: c, e, v2 :: v1 :: s, d ->
                    aux (c, e, `PAIR (v1, v2) :: s, d)

                | FST :: c, e, `PAIR (v1, _) :: s, d ->
                    aux (c, e, v1 :: s, d)

                | SND :: c, e, `PAIR (_, v2) :: s, d ->
                    aux (c, e, v2 :: s, d)

                | SUC :: c, e, `CST (Integer i) :: s, d ->
                    aux (c, e, `CST (Integer (succ i)) :: s, d)

                | PRE :: c, e, `CST (Integer i) :: s, d ->
                    aux (c, e, `CST (Integer (pred i)) :: s, d)

                | MUL :: c, e, `CST (Integer i1) :: `CST (Integer i2) :: s, d ->
                    aux (c, e, `CST (Integer (mult i1 i2)) :: s, d)

                | EQB :: c, e, `CST (Integer i1) :: `CST (Integer i2) :: s, d ->
                    let ieq = if i1 = i2 then zero else one in
                    aux (c, e, `CST ieq :: s, d)

                | EQB :: c, e, `CST b1 :: `CST b2 :: s, d ->
                    let ieq = if b1 = b2 then zero else one in
                    aux (c, e, `CST ieq :: s, d)

                | EQB :: c, e, v1 :: v2 :: s, d ->
                    let ieq = if v1 = v2 then zero else one in
                    aux (c, e, `CST ieq :: s, d)

                | ADD :: c, e, `CST (Integer i1) :: `CST (Integer i2) :: s, d ->
                    aux (c, e, `CST (Integer (add i1 i2)) :: s, d)

                | DIV :: c, e, `CST (Integer i1) :: `CST (Integer i2) :: s, d ->
                    aux (c, e, `CST (Integer (div i2 i1)) :: s, d)

                | MOD :: c, e, `CST (Integer i1) :: `CST (Integer i2) :: s, d ->
                    aux (c, e, `CST (Integer (i2 mod i1)) :: s, d)

                | SUB :: c, e, `CST (Integer i1) :: `CST (Integer i2) :: s, d ->
                    aux (c, e, `CST (Integer (sub i2 i1)) :: s, d)

                | MKA :: c, e, `CST (Integer n) :: s, d ->
                    let v = Array.make (CD.Intervals.V.get_int n) zero in
                    aux (c, e, `ARR v :: s, d)

                | GET :: c, e, `CST (Integer i) :: `ARR a :: s, d ->
                    let v = a.(CD.Intervals.V.get_int i) in 
                    aux (c, e, `CST v :: s, d)

                | SET::c,e,`CST(Integer i)::`ARR a::`CST v::s, d ->
                    let () =  Array.set a (CD.Intervals.V.get_int i) v in
                    aux (c, e, s , d)

                | IFZ (c1, _) :: c, e, `CST b :: s, d
                    when b = Primitives.zero ->
                    aux (c1 @ c, e, s, d)

                | IFZ (_, c2) :: c, e, _ :: s, d ->
                    aux (c2 @ c, e, s, d)

                | s -> s

            in aux (code, env, [], [])

    end


    open Transitions
    open MetricsDebug
    open Print

    let run_init code =
#ifndef BENCH
        let () = if !(run_params.debug) then print_endline "Run initialization" in
#endif
        run code []

    let finish = function
        | _, _, [v], _ -> v
        | [], _, `FAIL::_,_ -> `FAIL
        | c, _, s, d -> begin
            print_endline (show_bytecode 1 c);
            print_endline (show_stack s 1);
            print_endline (show_dump 1 d);
            failwith "unfinished computation"
            end
        (* | _ -> failwith "no return value" *)


    let wrap_run : bytecode -> parameters_structure -> unit =
        fun code params ->
#ifndef BENCH
            begin
            (if !(params.debug) then Printf.printf "Bytecode:\n%s" (show_bytecode 1 code));
            run_params.debug := !(params.debug);
            run_params.verbose := !(params.verbose);
            run_params.step_mode := !(params.step_mode);
            run_params.monitor := !(params.monitor);
            run_params.step_start := !(params.step_start)
            end;
#endif
            let v = finish (run_init code) in
#ifndef BENCH
            let () =
#endif
            print_string "- : " ; print_string @@ show_result v; print_endline ""
#ifndef BENCH
            in
            if !(params.monitor) then
                begin
                print_endline "\n===Monitor===\n=============\n";
                let met = run_params.metrics in
                let print_max (ls,fmt) = 
                    let (step_max, size_max) = max cmp_tuple ls in
                    Printf.printf fmt (string_of_int size_max) (string_of_int step_max) in
                let _ =
                List.map print_max
                [met.stack_sizes, "Stack max size:               %s at step %s\n";
                 met.dump_sizes, "Dump env max size:       %s at step %s\n";
                 met.dump_frames, "Dump length max :       %s at step %s\n";
                 met.dump_bounds, "Dump casts max :       %s at step %s\n"; 
                 met.longest_proxies, "Longest proxy size:           %s at step %s\n" ;
                 met.casts, "Largest amount of casts size: %s at step %s\n" ;
                 met.env_sizes, "Env max size: %s at step %s\n"]
                 in ();
                (* let () = print_endline "debug" in *)

                let instr_counts = met.instructions in
                let l_instr_counts = List.of_seq (MetricsEnv.to_seq instr_counts) in
                List.iter (fun (by, cnt) ->
                        printf "\n%i     %s" cnt (show_byte 0 by)) l_instr_counts;
                print_endline "\n=============\n============="; 
                run_params.metrics <- init_metrics ()
                end
#endif
end

module Machine = Make_Machine(Bytecode_Eager)
module Machine_Symbolic = Make_Machine(Bytecode_Symbolic)
module Machine_Symbolic_Cap = Make_Machine(Bytecode_Symbolic_Cap)
