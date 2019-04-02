module CD = Cduce_lib

type t = CD.Types.t
type var = CD.Var.t
type varset = CD.Var.Set.t
type subst = CD.Types.Subst.t
type b = CD.Types.const
type tau = CD.Types.t

let fresh_var () =
  let n = Oo.id (object end) in
  CD.Var.mk ~internal:false (Printf.sprintf "a%04d" n)

let fresh_dyn_id () =
  let n = Oo.id (object end) in
  Printf.sprintf "'d%04d" n

let fresh_dyn_var () =
  let n = Oo.id (object end) in
  CD.Var.mk ~internal:false (Printf.sprintf "d%04d" n)

let fresh_var_type () = CD.Types.var (fresh_var ())

(* let apply_subst theta t = CD.Types.Subst.full t theta

let simpl t =
  let vars = CD.Types.all_vars t in
  let mvars =
    vars |> CD.Var.Set.filter (fun v ->
      let t' = CD.Types.Subst.single t (v, CD.Types.empty) in
      not (CD.Types.equiv t t'))
  in
  let useless = CD.Var.Set.diff vars mvars |> CD.Var.Set.get in
  let t' = CD.Types.Subst.full_list t
    (List.map (fun v -> (v, CD.Types.empty)) useless)
  in
  (t', mvars)

let mvar t = snd (simpl t)

type scheme = {
  t: t;
  a: varset;
  mv: varset;
}

let scheme t a =
  let (t', mvars) = simpl t in
  { t = t'; a; mv = CD.Var.Set.diff mvars a }

let mono_scheme t =
  let (t', mvars) = simpl t in
  { t = t'; a = CD.Var.Set.empty; mv = mvars }

let gen_delta delta t =
  let (t', mvars) = simpl t in
  let v = CD.Types.all_vars t' in
  let a = CD.Var.Set.diff v delta in
  { t = t'; a; mv = CD.Var.Set.diff mvars a }

let poly_scheme = gen_delta CD.Var.Set.empty

let freshen t a =
  let theta =
    CD.Var.Set.get a
    |> List.map (fun v -> (v, fresh_var ()))
    (* |> List.map (fun v -> (v, CD.Var.refresh v)) *)
  in
  let a' = CD.Var.Set.from_list (List.map snd theta) in
  let t' = CD.Types.Subst.full_list t
    (List.map (fun (v, v') -> (v, CD.Types.var v')) theta)
  in
  (t', a')

let apply_subst_scheme theta { t; a; _ } =
  let (t', a') = freshen t a in
  let t'' = apply_subst theta t' in
  scheme t'' a'

let instance { t; a; _ } = fst (freshen t a)

let mvar_scheme { mv;_ } = mv

module Type_env = struct
  module M = Map.Make(struct 
                        type t = string
                        let compare = String.compare 
                      end)
  include M

  let find_opt x env = try Some (find x env) with Not_found -> None
  let update env1 env2 = fold add env2 env1
end

type type_env = scheme Type_env.t
type mono_env = t Type_env.t

let all_mvars env =
  Type_env.fold
    (fun _ { mv;_ } delta -> CD.Var.Set.cup delta mv) env CD.Var.Set.empty

let gen env = gen_delta (all_mvars env)
let gen_delta_mono_env delta = Type_env.map (gen_delta delta)
let gen_mono_env env = gen_delta_mono_env (all_mvars env)

let apply_subst_env theta = Type_env.map (apply_subst_scheme theta)
let apply_subst_mono_env theta = Type_env.map (apply_subst theta)

let cleaner =
  let rec pretty i acc =
    let ni,nm = i/26, i mod 26 in
    let acc = acc ^
      (String.make 1 (Char.chr (Char.code 'a' + nm)))
    in
    if ni == 0 then acc else pretty ni acc
  in
  let new_var counter =
    incr counter;
    CD.Var.mk ~internal:false (pretty !counter "")
    |> CD.Types.var
  in
  fun () ->
    let counter = ref (-1) in
    fun t ->
      let open CD.Types in
      if no_var t then t else
        let _tlv, pos, neg, all = collect_vars t in
        let theta =
          all
          |> CD.Var.Set.get
          |> List.map (fun v ->
              let is_pos = CD.Var.Set.mem pos v in
              let is_neg = CD.Var.Set.mem neg v in
              (v,
                if is_pos && is_neg then new_var counter
                else if is_pos then empty else any))
          in
          Subst.full_list t theta

let remove_useless_types ismoreprecise =
  let rec aux useful = function
    | t :: ts ->
        let useful' =
          if List.exists (fun t' -> ismoreprecise t' t) useful
          then useful
          else t :: List.filter (fun t' -> not (ismoreprecise t t')) useful
        in
        aux useful' ts
    | [] -> useful
  in
  function t :: ts -> aux [t] ts | [] -> assert false

let moregeneral t t' =
  (* Format.eprintf "t = %a\nt'=%a\nEND\n%!" CD.Types.Print.pp_type t CD.Types.Print.pp_type t'; *)
  (* let x = CD.Types.all_vars t' in *)
  (* Format.eprintf "x = %a\n%!" CD.Var.Set.print x; *)
  let res = CD.Type_tallying.is_squaresubtype (CD.Types.all_vars t') t t'
  in
  (* Format.eprintf "t = %a\nt'=%a\nres = %b\n%!" CD.Types.Print.pp_type t CD.Types.Print.pp_type t' res; *)
  res

let intersect_and_clean t thetas =
  (* Format.eprintf "here1\n\n%!"; *)
  let clean1 = cleaner () in
  let clean2 = cleaner () in
  let ts = List.map (fun theta -> apply_subst theta t) thetas in
  (* Format.eprintf "here2\n\n%!"; *)
  let ts' = remove_useless_types CD.Types.subtype ts in
  (* Format.eprintf "here3\n\n%!"; *)
  List.map clean1 ts'
  |> remove_useless_types CD.Types.subtype
  (* |> fun x -> (Format.eprintf "here4\n\n%!"; let y = remove_useless_types moregeneral x in Format.eprintf "here5\n\n%!"; y) *)
  |> remove_useless_types moregeneral
  |> List.fold_left CD.Types.cap CD.Types.any
  |> clean2

let pp_type =
  CD.Types.Print.pp_type

let pp_scheme ppf { t; a; _ } =
  if not (CD.Var.Set.is_empty a) then begin
    Format.fprintf ppf "forall";
    CD.Var.Set.iter (Format.fprintf ppf " %a" CD.Var.print) a;
    Format.fprintf ppf ". "
  end;
  pp_type ppf t

let pp_env' pp_content ppf env =
  Format.fprintf ppf "{ @[";
  Type_env.iter (fun x t -> Format.fprintf ppf "%s: %a,@ " x pp_content t) env;
  Format.fprintf ppf "@]}"

let pp_env = pp_env' pp_scheme
let pp_mono_env = pp_env' pp_type *)
