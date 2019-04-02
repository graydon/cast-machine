open Cast
open Syntax.SE_CDuce
open Print.Print
open Interpreter.Eager_Calculus

let rec repl () = 
    print_string "# ";
    let lb = Lexing.from_string (read_line ()) in
    let e = Parser.prog Lexer.token lb in
    (* print_string "prog: "; Print.Print.print_e e; print_endline ""; *)
    wrap_eval e;
    repl ()

let _ = repl ()