open Cast
open Syntax.SE_CDuce
open Print.Print
open Interpreter.Eager_Calculus
open Compile.Compile1
open Exec.Exec1
open Primitives

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
         | _ -> failwith "error: wrong argument"

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
    let e = Parser.prog Lexer.token lb in
    (* print_string "prog: "; Print.Print.print_e e; print_endline ""; *)
    eval_with_parameters params e;
    repl ()
  with Expression_Syntax_Error ->
    print_endline "error: expression syntax";
    repl ()
       | Type_Syntax_Error s ->
    print_endline @@ "error: type syntax " ^ s;
    repl ()
       | Empty_Program ->
    repl ()

let _ = repl ()