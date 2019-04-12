
type parameters_structure =
  {debug : bool ref;
   symbolic : string ref;
   machine: string ref;
   load_file : bool ref;
   verbose : int ref;
   step_mode : bool ref;
   step_start : int ref;
   monitor : bool ref}

let params =
  {debug = ref false;
  symbolic = ref "";
  machine = ref "machine";
  load_file = ref false;
  verbose = ref 1;
  step_mode = ref false;
  step_start = ref 0;
  monitor = ref false}

let rec find a x n =
if a.(n) = x then n 
else find a x (n+1)

let cmp_tuple (_,b) (_,d) =
  if b > d then 1 
  else if b < d then -1
  else 0

let printf = Printf.printf
let flush = Pervasives.flush stdout

let max cmp l = 
  let rec aux acc = function
  | [] -> acc
  | x :: t when cmp x acc > 0 -> aux x t
  | _ :: t -> aux acc t
  in aux (List.hd l) l