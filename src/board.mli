(** A representation of the gameboard *)

type t
(** The abstract type representing a gameboard *)

type loc
(** The type representation of the location of a single square *)

exception Mine

(* Operations *)

val empty : t
(** An empty board with no mines placed and no squares revealed *)

val set_n_mines : t -> int -> loc -> unit
(** [set_mines n board index] modifies [board] by placing [n] mines
    pseudorandomly throughout. There will not be a mine at the location
    denoted by [index]. Requires: input does not have any mines already
    set or squares revealed, [index] is a valid location for [board],
    and n is at least 0 and less than the number of empty tiles on the
    board *)

val flag : t -> loc -> unit
(** [flag board index] flips the flagged state of the square located at
    [index] in [board]. Requires: [index] is a valid location for
    [board] *)

val dig : t -> loc -> unit
(** [dig board index] digs (clears) the square located at [index] in
    [board]. Requires: [index] is a valid location for [board]. Raises
    [Mine] if the location being cleared contains a mine *)

(* Accessing Data *)

val pp_board : Format.formatter -> t -> unit
(** Pretty prints a board to the console *)