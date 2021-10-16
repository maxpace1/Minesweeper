type sq_type = Square.t

type t = sq_type Array.t Array.t

type loc = int * int

exception Mine

(* let empty = Array.make (30 * 16) Square.blank *)
let empty = Array.make_matrix 30 16 Square.blank

let custom_empty x y =
  if 1 <= x && x <= 100 && 1 <= y && y <= 100 then
    Array.make_matrix x y Square.blank
  else failwith "Bad Size Arguments"

let check_loc random_loc : loc = random_loc

let rec set_n_mines n b i =
  if i = 0 then () else failwith "Unimplemented"

let flag b i = b.(i) <- Square.flag b.(i)

let dig b i =
  b.(i) <-
    (try Square.dig b.(i) with
    | Square.Explode -> raise Mine)

let pp_board fmt b = failwith "Unimplemented"