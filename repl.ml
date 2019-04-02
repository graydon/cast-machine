open Cast

let rec repl () = 
    print_string "# ";
    let lb = Lexing.from_string (read_line ()) in
    let e = Parser.prog Lexer.token lb in
    print_endline "expression parsed: ";
    Print.Print.print_e e;
    print_endline "";
    repl ()


let _ = repl ()