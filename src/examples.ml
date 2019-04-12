(* open Types *)
open Syntax
open Primitives

include Interpreter
module CD = Cduce_lib


module Eager_Examples = struct
    open Syntax.Eager
    open Eager_Calculus

    open CD.Intervals.V
    (* Using CD.Intervals.V.zero etc. *)
    let zer = CD.Types.Integer (zero)
    let uno = CD.Types.Integer (succ zero)
    let deux = CD.Types.Integer (succ @@ succ zero)
    let un_et_deux = CD.Types.Pair (uno, deux)
    let dummy = var (mk_var "")

    let v1 = Var (fresh_var ())
    let f1 = Lam (qmark (), qmark (), mk_var "x", Cst zer)
    let cast1 = Cast (f1, (qmark (), dummy))

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

    let c1 = constant uno (* le type singleton 1 *)
    let c0 = constant zer
    let un_ou_deux = cup c0 c1 (* le type 1--2 *)

    let example_types = [
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


    (* Building blocks *)
    let cst_0 = Cst zer
    let cst_1 = Cst uno
    let x = mk_var "x"
    let y = mk_var "y"
    let succ e = Succ (e)
    let var e = Var (e)
    let cast e ti td = Cast (e, (ti, td))
    let fcast e t = Cast (e, (t, dom t))
    let lambda t1 t2 v e = Lam (t1, t2, v, e)
    let cst e = Cst e
    let app e1 e2 = App (e1, e2)

    let cast_cst_0 = Cast (cst_0, (qmark (), qmark ()))

    let id = Lam (qmark (), qmark (), x, Var (x))
    let f_succ = Lam (qmark (), qmark (), x, succ (var x))

    let at = mk_atom "true"
    let f0 = fcast (lambda (qmark ()) (qmark()) x (fcast cst_0 (qmark ()))) (cap (mk_arrow c0 c0) (mk_arrow c1 c1))
    let ex_f1 = app f0 cst_1
    let ex_f2 = app f0 cst_0

    let ex_s1 = Succ (fcast cst_0 c0)

    let om = lambda (qmark ()) (qmark ()) x (app (fcast (var x) (qfun ())) (var x))
    let ex_f3 = app om (fcast om (qmark ()))

    let lambda_dyn v e = lambda (qmark ()) (qmark ()) v e

    let f = mk_var "f"
    let church_0 = lambda (qmark ()) (qmark ()) f (lambda_dyn x (var x))
    let church_1 = lambda (qmark ()) (qmark ()) f (lambda_dyn x (app (var f) (var x)))

    (* let church_succ (Lam (_, _, f, (Lam (_, _, x, e)))) =
        let  *)

    let t0 = cap (mk_arrow c0 c0) (mk_arrow c1 c1)
    let t1 = mk_arrow (cup c0 c1) (cup c0 c1)
    let tc01 = cup c0 c1
    let f4 = (
        let y = mk_var "y" in
        let z = mk_var "z" in
        lambda t0 t1 y (lambda tc01 tc01 z (app (var y) (var z)))
    )
    let f4' = lambda_dyn x cst_1
    let ex_f4 = app (app f4 f4') cst_1



    let running_examples = [
            cst_0;
            cst_1;
            f1;
            lambda (qmark ()) (qmark ()) y f1;
            cst un_et_deux;
            app f1 cst_0;
            cast_cst_0;
            app f1 cast_cst_0;
            app id cst_0;
            app id id;
            app f_succ cst_0;
            app f_succ f_succ;
            ex_f1;
            ex_f2;
            ex_s1;
            Succ (fcast cst_0 empty);
            (* ex_f3; *)
            church_0;
            church_1;
            app f4 f4';
            ex_f4
            ]



    let eval_examples () = List.map wrap_eval running_examples

    (* let _ = eval_examples () *)
end

module Symbolic_Examples = struct
    open Syntax.Symbolic
    open Symbolic_Calculus
    open Print

    open CD.Intervals.V
    (* Using CD.Intervals.V.zero etc. *)
    let zer = CD.Types.Integer (zero)
    let uno = CD.Types.Integer (succ zero)
    let deux = CD.Types.Integer (succ @@ succ zero)
    let un_et_deux = CD.Types.Pair (uno, deux)
    let dummy = var (mk_var "")

    let v1 = Var (fresh_var ())
    let f1 = Lam (qmark (), qmark (), mk_var "x", Cst zer)
    let cast1 = Cast (f1, Cast (qmark ()))

    (* Building blocks *)
    let cst_0 = Cst zer
    let cst_1 = Cst uno
    let x = mk_var "x"
    let y = mk_var "y"
    let succ e = Succ (e)
    let var e = Var (e)
    let cast e ti td = Cast (e, Cast ti)
    let fcast e t = Cast (e, Cast t)
    let lambda t1 t2 v e = Lam (t1, t2, v, e)
    let cst e = Cst e
    let app e1 e2 = App (e1, e2)

    let cast_cst_0 = Cast (cst_0, Cast (qmark ()))

    let c1 = constant uno (* le type singleton 1 *)
    let c0 = constant zer

    let id = Lam (qmark (), qmark (), x, Var (x))
    let f_succ = Lam (qmark (), qmark (), x, succ (var x))
    let f0 = fcast (lambda (qmark ()) (qmark()) x (fcast cst_0 (qmark ()))) (cap (mk_arrow c0 c0) (mk_arrow c1 c1))
    let ex_f1 = app f0 cst_1
    let ex_f2 = app f0 cst_0


    let lambda_dyn v e = lambda (qmark ()) (qmark ()) v e
    let t0 = cap (mk_arrow c0 c0) (mk_arrow c1 c1)
    let t1 = mk_arrow (cup c0 c1) (cup c0 c1)
    let tc01 = cup c0 c1
    let f4 = (
        let y = mk_var "y" in
        let z = mk_var "z" in
        lambda t0 t1 y (lambda tc01 tc01 z (app (var y) (var z)))
    )
    let f4' = lambda_dyn x cst_1
    let ex_f4 = app (app f4 f4') cst_0

   


    let running_examples = [
            cst_0;
            cst_1;
            f1;
            lambda (qmark ()) (qmark ()) y f1;
            cst un_et_deux;
            app f1 cst_0;
            cast_cst_0;
            app f1 cast_cst_0;
            app id cst_0;
            app id id;
            app f_succ cst_0;
            app f_succ f_succ;
            ex_f1;
            ex_f2;
            app f4 f4';
            ex_f4
            ]

    let eval_symbolic_examples () = List.map wrap_eval running_examples

    (* let _ = eval_seymbolic_examples () *)
end


include Symbolic_Examples
include Eager_Examples


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
