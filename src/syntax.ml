(** Writing a functor for type/epxressions **)
include Types
open Primitives

module type Dynamic_Type = sig
    type var = Types.var
    type b = Types.b
    type t = Types.t (* static type *)
    type tau = Types.t (* dynamic type *)
    type subst = Types.subst
    val subst : t -> (var * t) list -> t
    val ceil : tau -> t
    val floor : tau -> t
end

module CDuce_Dynamic_Types : Dynamic_Type = struct
    type t = Types.t
    type var = Types.var
    type varset = Types.varset
    type subst = Types.subst
    type b = Types.b (* surely false *)
    (* type subst = CD.Types.Subst.t *)
    (* Idée: à la manière des schemes de Tommaso, 
    implémenter les types graduels comme des types
    t possédant des variables de types assignées à `Dyn 
    Je choisis cette version par rapport à celle utilisant 
    des atomes `Dyn, parce que CDuce permet de récupérer
    la variance des variables de type facilement, et aussi
    les variables de type permettent de les représenter de 
    manière distincte (avec des [fresh_var ()]) *)
    type tau = t

    let subst = CD.Types.Subst.full_list
    let subst_single = CD.Types.Subst.single
    let variance = CD.Types.Variable.variance
    let pp_type = CD.Types.Print.pp_type
    let apply = CD.Types.Arrow.apply (* [apply t1 t2 computes [t1 \circ t2] *)
    let get = CD.Types.Arrow.get
    let is_arrow t = subtype t CD.Types.Arrow.any

    let ceil t =
      let _, pos, neg, _ = collect_vars t in
      (* check if the first letter of the ident 
      is 'd', because then it is a dynamic typevar *)
      let t' = 
      subst t 
        (List.map 
          (fun v -> (v, any))
          (CD.Var.Set.get 
            (CD.Var.Set.filter (fun v ->
              (CD.Var.ident v).[0] = 'd') pos)))
      in
      subst t'
        (List.map 
          (fun v -> (v, empty))
          (CD.Var.Set.get 
            (CD.Var.Set.filter (fun v ->
              (CD.Var.ident v).[0] = 'd') neg)))
          
    let floor t =
      let _, pos, neg, _ = collect_vars t in
      (* check if the first letter of the ident 
      is 'd', because then it is a dynamic typevar *)
      let t' = 
      subst t 
        (List.map 
          (fun v -> (v, any))
          (CD.Var.Set.get 
            (CD.Var.Set.filter (fun v ->
              (CD.Var.ident v).[0] = 'd') neg)))
      in
      subst t'
        (List.map 
          (fun v -> (v, empty))
          (CD.Var.Set.get 
            (CD.Var.Set.filter (fun v ->
              (CD.Var.ident v).[0] = 'd') pos)))
end

module type Cast_Expr = sig
    include Dynamic_Type
    type p (* blame labels *)
    type alpha_vector
    type t_vector
    type e = 
      | Var of var
      | Cst of b
      | Lam of tau * tau * var * e
      | App of e * e
      | Cast of e * tau
      | TwoCast of e * tau * tau 
    (* type v *)
end

module type Cast_Language = sig
    include Cast_Expr
    (* other constructs of the language *)
    val is_value : e -> bool
    (* val pprint_e : [< `Var of string ]  -> string  *)
    (** TODO : find a way to define the signature of pprint_e and print_e
        that is compatible with the open variant used to define the function
        Here I found a way by adding a single constructor, but I would have like
        to use an "empty open variant" [< ] of which every open variant would be
        a supertype, and therefore by contravariance every function
        [variant -> string] would be a subtype of this type.
        On the other hand, now it becomes impossible to use pprint_e in practice,
        because if I use for example on a list 
            [ `Var "x", `Cst (`I 2) ] 
        which has type
            [> `Var of string | `Cst of b ]
        then this type is not a subtype of [< `Var of string ] *)
    (* val print_e : [< `Var of string ]  -> unit *)
end

(* dummy testing of the module signatures *)
(* module Test (Init_Type : Dynamic_Type) : (Dynamic_Type -> Cast_Expr) = 
    functor (DT : Dynamic_Type) ->
    struct
        include DT
        type t_vector = int
        type alpha_vector = int
        type e = int
        type v = int
        type p = int
    end *)

module type Make_Cast_Expr = Dynamic_Type -> Cast_Expr

(* Victor's types and expressions from "Space-efficient [...]" notes *)

(* mdodule SE_Types = struct 
    (* type var = string *)
    type var = string
    type t = 
      [ | `TVar of var
        | `Int 
        | `Bool
        | `Arr of t * t
        | `Or of t * t
        | `And of t * t
        | `Neg of t
        | `Empty ]
    type b = [ `I of int | `B of bool ]
    type tau = 
      [ | `Dyn
        | `TVar of var
        | `Int 
        | `Bool
        | `Arr of tau * tau
        | `Or of tau * tau
        | `And of tau * tau
        | `Neg of tau
        | `Empty ]
end *)

module Make_SE (Init_Type : Dynamic_Type) : Cast_Expr = struct
    include Init_Type
    type alpha_vector = Init_Type.var list
    type t_vector = Init_Type.t list
    type p =    (* blame label *)
        [ | `Simple of int
          | `Pos of int * int
          | `Neg of int * int ]
(* hi *)
    type e = 
      | Var of var
      | Cst of b
      | Lam of tau * tau * var * e
      | App of e * e
      | Cast of e * tau
      | TwoCast of e * tau * tau 
        (* for now no product, let and type abstraction *)
        (* | `Prd of e * e *)
        (* | `Pi1 of e *)
        (* | `Pi2 of e *)
        (* | `Let of var * e * e *)
        (* | `TLam of alpha_vector * e *)
        (* | `TApp of e * t_vector *)
end

module Make_Cast_Language (Init_Type : Dynamic_Type) : (Make_Cast_Expr -> Cast_Language) = 
    functor (MCE : Make_Cast_Expr) -> 
    struct
        include MCE(Init_Type)
        let is_value = fun _ -> true
end

module SE_CDuce = Make_Cast_Language(CDuce_Dynamic_Types)(Make_SE)




(* module POPL19_Types = struct
    type var = string
    type b = [ `I of int | `B of bool ]
    type t = [
        | `TVar of var
        | `Int 
        | `Bool
        | `Prod of t * t 
        | `Arr of t * t ]
    type tau = [ 
        | `Dyn
        | `TVar of var
        | `Int 
        | `Bool
        | `Prod of tau * tau
        | `Arr of tau * tau ]
end *)

(* module Make_POPL19 (Init_Type : Dynamic_Type) : Cast_Expr = struct
    (** POPL19 types and cast expressions *)
    include Init_Type
    type alpha_vector = Init_Type.var list
    type t_vector = Init_Type.t list
    type p =    (* blame label *)
            [ | `Simple of int
            | `Pos of int * int
            | `Neg of int * int ]
    type e = 
      [ | `Var of var
        | `Cst of b
        | `Lam of tau * tau * var * e
        | `App of e * e
        | `Prd of e * e
        | `Pi1 of e
        | `Pi2 of e
        | `Let of var * e * e
        | `TLam of alpha_vector * e
        | `TApp of e * t_vector
        | `Cast of e * tau * p * tau ]
    (* type v = int *)
    (* type env = int *)
    (* let create () = 0 *)
    end *)

(* module POPL19 = (Make_Cast_Language(POPL19_Types))(Make_POPL19)
module SE = (Make_Cast_Language(SE_Types))(Make_SE) *)