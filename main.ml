open Interpreter.Eager_Calculus
open Primitives
open Errors
open Lexing
open Utils
let wrap_abstract = fun _ -> ()


let () = if Array.length (Sys.argv) > 1 then begin
      (if Array.mem "--help" Sys.argv then (print_endline "No help" ; raise Exit));
      (if Array.mem "--abstract" Sys.argv then params.abstract := true);
      (if Array.mem "--interpreter" Sys.argv then params.machine := "");
      (if Array.mem "--machine" Sys.argv then params.machine := "machine");
      (if Array.mem "--symbolic" Sys.argv then params.symbolic := "symbolic");
      (if Array.mem "--load" Sys.argv then params.load_file := true);
      (if Array.mem "--debug" Sys.argv then params.debug := true);
      (if Array.mem "--verbose" Sys.argv then 
      let i = find Sys.argv "--verbose" 0 in params.verbose := (int_of_string @@ Sys.argv.(i+1)));
      (if Array.mem "--stepmode" Sys.argv then 
      begin params.step_mode := true;
        let i = find Sys.argv "--stepmode" 0 in
        try let id = int_of_string (Sys.argv.(i+1)) in 
          params.step_start := id
        with _ -> ()
      end);
      (if Array.mem "--monitor" Sys.argv then params.monitor := true) end

let eval_with_parameters params e =
  let () = if !(params.debug) 
  then (print_string "Program: "; print_e e; print_endline "") in
  if !(params.machine) != "" then
  begin
    if !(params.symbolic) != "" then 
      let open Exec.Machine_Symbolic in 
      let open Compile.Compile_Symbolic in begin
      let () = if !(params.debug) then print_endline "Compiling.." in
      let btc = compile Nil e in
      let () = if !(params.debug) then print_endline "Running bytecode.." in
      wrap_run btc params end
    else 
      let open Exec.Machine in 
      let open Compile.Compile in begin
      let () = if !(params.debug) then print_endline "Compiling.." in
      let btc = compile Nil e in
      let () = if !(params.debug) then print_endline "Running bytecode.." in
      wrap_run btc params end

  end
  else if !(params.abstract) then wrap_abstract e
  else wrap_eval e

let user_input = ref ""
let first = ref true

let rec repl () = 
  try
    if !first then print_string @@ !(params.symbolic) ^ !(params.machine) ^ "# ";
    let nl = read_line () in
    let tnl = String.trim nl in
    let ltnl = String.length tnl in
    let () = user_input := !user_input ^ " " ^ nl in
    begin match String.sub tnl (ltnl-2) 2 with
    | ";;" ->
      let lb = Lexing.from_string !user_input in
      let prog = Parser.prog Lexer.token lb in
      begin match prog with
      | Eol -> failwith "error: didn't expect eol"
      | Expr e -> 
          (user_input := ""; first := true;
          eval_with_parameters params e) end;
      repl ()
    | _ ->
      first := false;
      repl () 
    end
  with Expression_Syntax_Error ->
    print_endline "error: expression syntax";
    user_input := "";
    repl ()
       | Type_Syntax_Error s ->
    print_endline @@ "error: type syntax " ^ s;
    user_input := "";
    repl ()
       | Empty_Program ->
    repl ()
       | Parser.Error ->
    print_endline @@ "error: can't parse program {" ^ !user_input ^ "}";
    user_input := "";
    repl () 
  

(* parse a lexbuf, and return a more explicit error when it fails *)
let parse_buf_exn lexbuf params =
  try
    Parser.prog Lexer.token lexbuf
  with _ ->
    begin
      let tok = Lexing.lexeme lexbuf in
      raise (send_parsing_error (Lexing.lexeme_start_p lexbuf) ((!(params.machine)) ^ tok))
    end

(* extract a line from a lexbuf . Load file when necessary *)
let rec extract_line lexbuf acc params = 
  let program = parse_buf_exn lexbuf params
  in begin
    match program with
    | Eol ->  true, acc
    (* | Open (file, _)  -> false, ((get_code file params) @ acc) *)
    | x  -> false, x :: acc
  end

and get_code file_name = begin
    let lexbuf = Lexing.from_channel @@ open_in file_name
    in let pos = lexbuf.Lexing.lex_curr_p 
    in let pos = {pos_bol = pos.Lexing.pos_cnum; 
                  pos_fname = pos.Lexing.pos_fname; 
                  pos_lnum = pos.Lexing.pos_lnum;
                  pos_cnum = pos.Lexing.pos_cnum;}

    in let _ = lexbuf.lex_curr_p <- {
        pos_bol = 0;
        pos_fname = file_name;
        pos_lnum = 1;
        pos_cnum = 0;
      }

    in let rec aux acc =  begin
        let reached_eof, l = extract_line lexbuf acc params
        in if reached_eof then
          l
        else aux l
      end
    in let code = begin
        try
          aux []
        with _ ->
          let _ = Lexing.flush_input lexbuf
          in let _ = Parsing.clear_parser ()
          in let _ = print_endline "parsing_error" in []
      end

    in let _ = lexbuf.lex_curr_p <- {pos_bol = pos.pos_bol;
                                     pos_fname = pos.pos_fname;
                                     pos_lnum = pos.pos_lnum;
                                     pos_cnum = pos.pos_cnum;
                                    }
    in code
end

let _ =
  if !(params.load_file) then
    let prog = get_code Sys.argv.(2) in
    List.iter
    (function
    | Expr e -> eval_with_parameters params e 
    | Eol -> failwith "uncorrect file loaded")
    prog
  else
    repl ()