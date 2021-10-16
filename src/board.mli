(** A representation of the gameboard *)

type t
(** The abstract type representing a gameboard *)

type loc
(** The type representation of the location of a single square *)

exception Mine

(* Operations *)

val empty : t
(** An empty board with no mines placed and no squares revealed *)

val set_mines : t -> loc -> t
(** [set_mines board index] returns a board with 99 mines placed
    pseudorandomly throughout the board. There will not be a mine at the
    location denoted by [index]. Requires: input does not have any mines
    already set or squares revealed, and [index] is a valid location for
    [board] *)

val flag : t -> loc -> t
(** [flag board index] flips the flagged state of the square located at
    [index] in [board] and returns the new board. Requires: [index] is a
    valid location for [board] *)

val dig : t -> loc -> t
(** [dig board index] digs (clears) the square located at [index] in
    [board] and returns the new board. Requires: [index] is a valid
    location for [board]. Raises [Mine] if the location being cleared
    contains a mine *)

(* Accessing Data *)

val pp_board : Format.formatter -> t -> unit
(** Pretty prints a board to the console *)