type t = {
  mines_around : int;
  is_mine : bool;
  flag_mine : bool;
  dug_up : bool;
}

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
