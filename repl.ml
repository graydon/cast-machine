open Cast
open Print.Print
open Interpreter.Eager_Calculus
open Exec.Exec1
open Primitives
open Errors
open Lexing

type parameters_structure =
  {debug : bool ref;
   symbolic : string ref;
   machine: string ref}

let params =
  {debug = ref true;
  symbolic = ref "";
  machine = ref ""}

let () = if Array.length (Sys.argv) > 1
         then match Sys.argv.(1) with
         | "--machine" -> params.machine := "machine mode"
         | "--symbolic" -> params.symbolic := "symbolic"
         | "--load" -> params.machine := "machine mode"
         | _ -> ()

let eval_with_parameters params e =
  let () = if !(params.debug) then print_e e; print_endline "" in
  if !(params.machine) = "" then wrap_eval e
  else
      let btc = compile e in
      wrap_run btc

let rec repl () = 
  try
    print_string @@ !(params.symbolic) ^ !(params.machine) ^ "# ";
    let lb = Lexing.from_string (read_line ()) in
    let prog = Parser.prog Lexer.token lb in
    begin match prog with
    | Eol -> failwith "didn't expect eol"
    | Expr e -> eval_with_parameters params e end;
    (* print_string "prog: "; Print.Print.print_e e; print_endline ""; *)
    repl ()
  with Expression_Syntax_Error ->
    print_endline "error: expression syntax";
    repl ()
       | Type_Syntax_Error s ->
    print_endline @@ "error: type syntax " ^ s;
    repl ()
       | Empty_Program ->
    repl ()

(* parse a lexbuf, and return a more explicit error when it fails *)
let parse_buf_exn lexbuf params =
  try
    Parser.prog Lexer.token lexbuf
  with exn ->
    begin
      let tok = Lexing.lexeme lexbuf in
      raise (send_parsing_error (Lexing.lexeme_start_p lexbuf) tok)
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
  if Array.length Sys.argv > 2 && Sys.argv.(1) = "--load" then
    let prog = get_code Sys.argv.(2) in
    List.iter
    (function
    | Expr e -> eval_with_parameters params e 
    | Eol -> failwith "uncorrect file loaded")
    prog
  else
    repl ()