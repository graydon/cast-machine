(* TODO: fonctoriser ce truc pour lui donner un type tau générique *)
(* TODO: se brancher sur le système de types de CDuce *)
open Types
module CD = Cduce_lib

let fresh_var () =
  let n = Oo.id (object end) in
  CD.Var.mk ~internal:false (Printf.sprintf "a%04d" n)


module CDuce_Dynamic_Types : Dynamic_Type = struct
    type t = CD.Types.t
    type var = CD.Var.t
    type varset = CD.Var.Set.t
    type subst = CD.Types.Subst.t


    (* Idée: à la manière des schemes de Tommaso, 
    implémenter les types graduels comme des types
    t possédant des variables de types assignées à `Dyn *)

    type tau = {
    t: t;
    a: varset; (* type variables *)
    dv: varset; (* dynamic variables *)
    }
end

module SE_CDuce = (Make_Cast_Language(CDuce_Dynamic_Types))(Make_SE)
