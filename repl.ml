open Cast
open Syntax.SE_CDuce
open Print.Print
open Interpreter.Eager_Calculus
open Compile.Compile1
open Exec.Exec1

type parameters_structure =
  {debug : bool ref;
   machine: string ref}

let params =
  {debug = ref false;
  machine = ref ""}

let () = if Array.length (Sys.argv) > 1
          then 
          params.machine := "machine mode"

let eval_with_parameters params e =
  if !(params.machine) = "" then wrap_eval e
  else 
      let btc = compile e in
      wrap_run btc

let rec repl () = 
    print_string @@ !(params.machine) ^ "# ";
    let lb = Lexing.from_string (read_line ()) in
    let e = Parser.prog Lexer.token lb in
    (* print_string "prog: "; Print.Print.print_e e; print_endline ""; *)
    eval_with_parameters params e;
    repl ()

let _ = repl ()