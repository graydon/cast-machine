open Types

type e'_eval = [ e' | `Fail ]

let reduce = function
    | `TwoCast (`TwoCast (`Lam (tau1, tau2, x, e), tau_i, tau_d), tau_i', tau_d')           (* Seq *)
        -> `TwoCast (`Lam (tau1, tau2, x, e), `And (tau_i, tau_i'), `And (tau_d, tau_d'))
    | _ -> failwith "Not implemented or can't reduce"
    (**TODO**)
    (* | `TwoCast (`Cst c, tau1, tau2) when true -> `Cst c 
    | `TwoCast (`Cst c, tau1, tau2) when false -> `Fail
    | `App (`TwoCast (`Lam (tau1, tau2, x, e), tau_i, tau_d), v) when is_v v -> `Fail  *)