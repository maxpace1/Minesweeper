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

let mines_ok mines_around =
  assert (
    (*Normal behavior*) (0 <= mines_around && mines_around <= 8)
    || (*Blank tile*) mines_around = -1)

let rep_ok t =
  mines_ok t.mines_around;
  assert (not (t.is_mine && t.dug_up))

let return_rep_ok t =
  rep_ok t;
  t

let blank : t =
  {
    mines_around = -1;
    is_mine = false;
    flag_mine = false;
    dug_up = false;
  }

let create_square (mine : bool) (around : int) : t =
  mines_ok around;
  return_rep_ok { blank with mines_around = around; is_mine = mine }

let flag (sq : t) =
  rep_ok sq;
  return_rep_ok { sq with flag_mine = not sq.flag_mine }

let dig (sq : t) =
  rep_ok sq;
  if sq.dug_up || sq.flag_mine then raise NoOperationPerformed
  else if sq.is_mine then raise Explode
  else return_rep_ok { sq with dug_up = true }

let ok_checker (sq : t) (adj_sq_list : t list) : bool =
  sq.mines_around = -1
  ||
  (rep_ok sq;
   List.length
     (List.filter
        (fun sq ->
          rep_ok sq;
          sq.is_mine)
        adj_sq_list)
   = sq.mines_around)

let get_dug (sq : t) =
  rep_ok sq;
  sq.dug_up

let get_flag (sq : t) =
  rep_ok sq;
  sq.flag_mine

let get_val (sq : t) =
  rep_ok sq;
  if sq.dug_up && not sq.is_mine then Some sq.mines_around else None

(* TODO REMOVE (debug purposes only) *)
let test_print (sq : t) =
  rep_ok sq;
  if sq.is_mine then "*"
  else
    let k = sq.mines_around in
    string_of_int k
