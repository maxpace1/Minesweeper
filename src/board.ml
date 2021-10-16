type t = Square.t Array.t

type loc = int

exception Mine

let empty = Array.make (30 * 16) Square.blank

let rec set_n_mines n b i =
  if i = 0 then () else failwith "Unimplemented"

let flag b i = b.(i) <- Square.flag b.(i)

let dig b i =
  b.(i) <-
    (try Square.dig b.(i) with
    | Square.Explode -> raise Mine)

let pp_board fmt b = failwith "Unimplemented"