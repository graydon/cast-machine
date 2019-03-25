(* open Types *)
open Syntax
open Primitives
open Syntax.SE_CDuce
open Print
include Interpreter
module CD = Cduce_lib

open CD.Intervals.V
open Eager_Calculus


(* let is_dynamic t = CD.Types.Atom.has_atom t dyn_atom *)
(* from CD.Intervals.V *)
let zer = CD.Types.Integer (CD.Intervals.V.zero)
let uno = CD.Types.Integer (succ zero)
let deux = CD.Types.Integer (succ @@ succ zero)
let un_et_deux = CD.Types.Pair (uno, deux)

let vfresh = Var (fresh_var ())
let f1 = Lam (qmark (), qmark (), mk_var "x", Cst zer)
let cast1 = Cast (f1, qmark ())

(* SE with CDuce types *)
let examples = 
    [ Var (mk_var "x");
      Cst zer;
      Cst uno;
      f1;
      Lam (qmark (), qmark (), mk_var "y", f1);
      cast1;
      Cst un_et_deux;
    ]

let c1 = constant uno
let c0 = constant zer
let un_ou_deux = cup c0 c1

let example_types =
    [
        qmark ();
        qfun ();
        constant un_et_deux;
        c1;
        un_ou_deux
    ]

let show_examples () =
    print_string "Cast expressions with CDuce types:\n";
    List.iter (fun e -> print_e e; print_endline "") examples

let show_example_types () =
    print_string "Dynamic types using CDuce types:\n";
    List.iter (fun t -> print_t t; print_endline "") example_types

let i0 = Cst zer
let i1 = Cst uno
let cast0 = TwoCast (i0, qmark (), qmark ())

let running_examples = 
        [
        i0;
        i1;
        f1;
        Lam (qmark (), qmark (), mk_var "y", f1);
        Cst un_et_deux;
        App (f1, i0);
        cast0;
        App (f1, cast0)
        ]

let eval_examples () = List.map wrap_eval running_examples

let _ = eval_examples ()

(* 
open Old_print

let tt = `Cst (`B true)
let ff = `Cst (`B false) 
let one = `Cst (`I 1)
let zero = `Cst (`I 0)
let al = "α"
let beta = "β"
let x = `Var "x"
let dyn_fun = `Arr (`Dyn, `Dyn)


(* POPL19 with naive types *)
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

(* SE with naive types *)
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
 *)
