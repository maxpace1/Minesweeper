type t = {
  mines_around : int;
  is_mine : bool;
  flag_mine : bool;
  dug_up : bool;
}

(* AF: If a square is blank or flag (to user), [dug_up] will be false.
   [flag_mine] represents a flagged mine -> has no effect if square has
   been dug up. [mines_around] shows how many mines surround the current
   square to display to the user if the square has been dug up and is
   not a mine.

   RI: 0 <= [mines_around] <= 8. [is_mine] && [dug_up] false. *)

exception NoOperationPerformed

exception Explode

let blank : t =
  {
    mines_around = -1;
    is_mine = false;
    flag_mine = false;
    dug_up = false;
  }

let create_square (mine : bool) (around : int) : t =
  { blank with mines_around = around; is_mine = mine }

let flag (sq : t) = { sq with flag_mine = not sq.flag_mine }

let dig (sq : t) =
  if sq.dug_up || sq.flag_mine then raise NoOperationPerformed
  else if sq.is_mine then raise Explode
  else { sq with dug_up = true }

let get_dug (sq : t) = sq.dug_up

let get_flag (sq : t) = sq.flag_mine

let get_val (sq : t) =
  if sq.dug_up && not sq.is_mine then Some sq.mines_around else None

(* TODO REMOVE (debug purposes only) *)
let test_print (sq : t) =
  if sq.is_mine then "m" else string_of_int sq.mines_around
