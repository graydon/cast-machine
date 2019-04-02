module CD = Cduce_lib

open Stream

let print = CD.Types.Print.printf

(* let st = of_string "0 | 1"
let ast = CD.Parser.pat st
let node_t = CD.Typer.typ CD.Typer.empty_env ast
let t = CD.Types.descr node_t *)

(* transform a string into a cduce type *)
let parse_t str = 
    print_string str;
    str |> Stream.of_string |> CD.Parser.pat 
        |> CD.Typer.typ CD.Typer.empty_env |> CD.Types.descr

(* let filename = Sys.argv.(1) *)
(* 
let main () =
  let input = open_in filename in
  let filebuf = Lexing.from_channel input in
  try
    Parser.main Lexer.token filebuf
  with
  | Lexer.Error msg ->
      Printf.eprintf "%s%!" msg
  | Parser.Error ->
      Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start filebuf)
  ;
    IO.close_in input *)