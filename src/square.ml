type t = {
  mines_around : int;
  is_mine : bool;
  mark_mine : bool;
  dug_up : bool;
}

exception AlreadyDugUp

exception NotDugUp

exception Explode

let blank : t =
  {
    mines_around = -1;
    is_mine = false;
    mark_mine = false;
    dug_up = false;
  }

let create_square (mine : bool) (around : int) : t =
  { blank with mines_around = around; is_mine = mine }

let flag (sq : t) = { sq with mark_mine = not sq.mark_mine }

let dig (sq : t) =
  if sq.dug_up then raise AlreadyDugUp
  else if sq.is_mine then raise Explode
  else { sq with dug_up = true }

let get_dug (sq : t) = sq.dug_up

let get_mark (sq : t) = sq.mark_mine

let get_val (sq : t) =
  if sq.dug_up && not sq.is_mine then Some sq.mines_around else None
