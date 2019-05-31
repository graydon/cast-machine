open Stream
open Sys


type 'a stream = Cons of 'a * (unit -> 'a stream)

let unfold = function
  | Cons (x, st) -> (x, st ())

let rest = function
  | Cons (_,st) -> st ()

let rec count_from n = Cons (n, fun () -> count_from (n+1))

let rec sift n st =
  let (hd,tl) = unfold st in
  (* let () = Printf.printf "Head:%i\n" hd in *)
    if hd mod n = 0 then sift n tl
    else Cons (hd, fun () -> sift n tl)

let rec sieve st =
  let (hd,tl) = unfold st in
    Cons (hd, fun () -> sieve (sift hd tl))

let rec get (Cons (x, st)) = function
  | 0 -> x
  | i -> get (st ()) (i-1)

let primes = sieve (count_from 2)

let time f st i =
  let t = Sys.time() in
  let fx = f st i in
  Printf.printf "execution time: %fs\n" (Sys.time() -. t);
  fx

let _ = time get primes (int_of_string(Sys.argv.(1)))
