open Types

let () = print_string "ah\n"

let tt = Cst (`B true)
let ff = Cst (`B false) 

let _ =
    let examples = 
        [
            Var "x";
            Cst (`I 2);
            Cst (`B true);
            Lam (`Int, `Int, "x", Var "x");
            Lam (`Dyn, `Bool, "x", tt);
            Prd (tt, ff)
        ] in
    List.iter (fun e -> print_e e; print_endline "") examples