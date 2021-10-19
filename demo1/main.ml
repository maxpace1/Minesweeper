open Game
open Board

let main () =
  let my_board = Board.set_mines (30, 16) 99 (15, 8) in
  dig my_board (15, 8);
  pp_board my_board

(* Execute demo. *)
let () = main ()
