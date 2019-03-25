open Syntax
open Types
open Primitives

module CDuce_Dynamic_Types = struct
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


module SE_CDuce = Make_Cast_Language(CDuce_Dynamic_Types)(Make_SE)

