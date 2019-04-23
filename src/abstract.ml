(* open Types *)
open Syntax
open Primitives
open Types.Print

module Abstract = struct
  include Eager

  module Env = struct 
        include Hashtbl.Make(struct 
            type t = var
            let equal = (=)
            let hash = Hashtbl.hash
        end)
    end

  type c = tau * tau

  (* a bit hacky, done in order to define u, v and env properly *)
  type 'a au = [ `Cst of b | `Cls of var * e * 'a ]
  type 'a av = [ 'a au | `Cast of ('a au) * c ]

  type env = (env av) Env.t
  and u = (env au)
  and v = (env av)

  let env_empty () = Env.create 20

  type ctxt = Nothing | LeftSquare of e * ctxt | RightSquare of v * ctxt | Cast of c * ctxt

  type sigma = SNothing | Frame of ctxt * env * kappa
  and kappa  = Sigma of sigma | Bound of c * sigma
  (* type 'b id = 'b *)
  (* type 'a akap = [ `Cast of c * ('a asig) ]
  type kappa = kappa akap
  and sigma = kappa asig *)

  let comp : c -> c -> c = fun (t1,t2) (t1',t2') -> (cap t1 t1', cap t2 t2')
  
  let applycast : c -> v -> v = fun _ u -> u

  let cast : c -> kappa -> kappa = fun c -> function
  | Sigma s -> Bound (c, s)
  | Bound (d, s) -> Bound (comp c d, s)

  type state = e * env * ctxt * kappa
  type val_state = v * env * ctxt * kappa

  let rec term_step : state -> v = fun (e, rho, ctx, ka) ->
    match e with
    | Cst k ->                val_step (`Cst k, rho, ctx, ka)
    | Var x ->                val_step (Env.find rho x, rho, ctx, ka)
    | Lam (_, x, m) ->        val_step (`Cls (x, m, rho), rho, ctx, ka)
    | App (l, m)    ->        term_step (l, rho, LeftSquare (m, ctx), ka)
    | Cast (m, c) 
      when ctx = Nothing ->   term_step (m, rho, Nothing, cast c ka)
    | Cast (m, c) ->          term_step (m, rho, Cast (c, ctx), ka)
    | _ -> failwith "not implemented"

  and val_step : val_state -> v = fun (v, rho, ctx, ka) -> 
    begin match ctx with
    | Nothing -> 
        begin match ka with 
        | Sigma SNothing ->                       v
        | Sigma (Frame (ctx, rho', ka)) ->        val_step (v, rho', ctx, ka)
        | Bound (c, s) ->                         val_step (v, rho, Cast (c, Nothing), Sigma s)
        end       
    | LeftSquare (m, ctx) ->                      term_step (m, rho, RightSquare (v, ctx), ka)
    | RightSquare (`Cls (x, m, rho'), Nothing) -> let () = Env.replace rho' x v in 
                                                  term_step (m, rho', Nothing, ka)
    | RightSquare (`Cls (x, m, rho'), ctx)     -> let () = Env.replace rho' x v in
                                                  term_step (m, rho', ctx, Sigma (Frame (ctx, rho, ka)))
    | Cast (c, ctx) ->                            let v' = applycast c v in 
                                                  val_step (v', rho, ctx, ka)
    | _ -> failwith "not implem"
    end

  let show_cast (c1, c2) =
    Printf.sprintf "(%s, %s)" (pp_tau c1) (pp_tau c2)

  let rec show_v = function
    | `Cst c -> pp_b c
    | `Cls _ -> "closure"
    | `Cast (u, c) -> Printf.sprintf "%s<%s>" (show_v u) (show_cast c)
  
  let wrap_abstract : e -> unit = fun e -> 
    let _ = term_step (e, env_empty (), Nothing, Sigma SNothing) in print_string ""
    (* in print_endline (show_v v) *)

end