open Types
open Casts

let tt = `Cst (`B true)
let ff = `Cst (`B false) 
let one = `Cst (`I 1)
let zero = `Cst (`I 0)
let al = "α"
let beta = "β"
let x = `Var "x"
let dyn_fun = `Arr (`Dyn, `Dyn)


let _ =
    let examples = 
        [
            `Var "x";
            `Cst (`I 2);
            `Cst (`B true);
            `Lam (`Int, `Int, "x", `Var "x");
            `Lam (`Dyn, `Bool, "x", tt);
            `App (`Lam (`Int, `Int, "x", `Var "x"), zero);
            `Prd (tt, ff);
            `Pi1 (`Prd (tt, ff));
            `Pi2 (`Prd (ff, tt));
            `Let ("x", tt, `Var "x");
            `TLam ([al; beta], `Lam (`TVar al, `TVar beta, "x", `Var "x"));
            `Cast (`Lam (`Dyn, `Dyn, "x", x), dyn_fun, `Simple 0, `Arr (`Int, `Int));
            `Cast (`Cast (`Lam (`Dyn, `Dyn, "x", x), dyn_fun, `Simple 0, `Arr (`Int, `Int)), `Arr (`Int, `Int), `Simple 1, dyn_fun)
        ] in
    print_string "Cast expressions:\n";
    List.iter (fun e -> print_e e; print_endline "") examples

let _ = 
    let examples = 
        [
            `Var "x";
            `Lam (`Empty, `Empty, "x", x);
            (* `Cst (`I 2);
            `Cst (`B true);
            `Lam (`Int, `Int, "x", `Var "x");
            `Lam (`Dyn, `Bool, "x", tt);
            `App (`Lam (`Int, `Int, "x", `Var "x"), zero);
            `Prd (tt, ff);
            `Pi1 (`Prd (tt, ff));
            `Pi2 (`Prd (ff, tt));
            `Let ("x", tt, `Var "x");
            `TLam ([al; beta], `Lam (`TVar al, `TVar beta, "x", `Var "x")); *)
            `TwoCast (`Lam (`Dyn, `Dyn, "x", x), dyn_fun, `Arr (`Int, `Int));
            `TwoCast (`TwoCast (`Lam (`Dyn, `Dyn, "x", x), dyn_fun, `Arr (`Int, `Int)), `Arr (`Int, `Int), dyn_fun)
        ] in
    print_string "\nOther cast expressions:\n";
    List.iter (fun e -> print_e e; print_endline "") examples