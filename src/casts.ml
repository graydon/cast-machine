open Syntax
open Types

module CDuce_Dynamic_Types = struct
    type t = Types.t
    type var = Types.var
    type varset = Types.varset
    type subst = Types.subst
    type b = Types.b (* surely false *)
    (* type subst = CD.Types.Subst.t *)


    (* Idée: à la manière des schemes de Tommaso, 
    implémenter les types graduels comme des types
    t possédant des variables de types assignées à `Dyn *)

    type tau = {
    t: t;
    a: varset; (* type variables *)
    dv: varset; (* dynamic variables *)
    }    

    let subst = CD.Types.Subst.full_list

    let subtype = CD.Types.subtype
    let pp_type = CD.Types.Print.pp_type
    let cap = CD.Types.cap
    let dom = CD.Types.Arrow.domain
    let apply = CD.Types.Arrow.apply (* [apply t1 t2 computes [t1 \circ t2] *)
    let get = CD.Types.Arrow.get
    let is_arrow t = subtype t CD.Types.Arrow.any

    let ceil_naive {t; dv; _} = 
      subst t 
        @@ List.map (fun v -> (v, CD.Types.any)) (CD.Var.Set.get dv)

    (* let ceil t =
      if is_arrow t then
        let (dom, arr) = get t in *) (* to continue *)

    let floor_naive {t; dv; _} = 
      subst t 
        @@ List.map (fun v -> (v, CD.Types.empty)) (CD.Var.Set.get dv)

end

module SE_CDuce = Make_Cast_Language(CDuce_Dynamic_Types)(Make_SE)

