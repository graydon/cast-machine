open Primitives
(** Type structures and functions to benchmark **)

type t = CD.Typer.t

let pp_env = CD.Typer.pp_env
let show_env e = pp_env (Format.formatter_of_out_channel stdout) e
let parse_typedefs s = CD.Parser.prog (Stream.of_string s)
let type_defs = CD.Typer.type_defs;;

let collect_types tl = List.map
(fun (e : CD.Ast.pmodule_item) -> match e with
    | { descr = CD.Ast.TypeDecl (x,pl,t) ; _ } -> (x,pl,t)
    | _ -> failwith "not a typedecl") tl

let add_typedefs env s =
  let ctd = parse_typedefs s |> collect_types in 
  type_defs env ctd

(** builtin type Stream **)
let builtins = 
  ["type Stream('a) = ('a, ([] -> Stream('a)))";
    "type Tree = (Int, (Tree, Tree)) | (Int, `nil) | `nil"]