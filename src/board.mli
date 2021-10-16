(** A representation of the gameboard *)

type t
(** The abstract type representing a gameboard *)

type loc = int * int
(** The type representation of the location of a single square *)

exception Mine

(* Operations *)

val empty : t
(** [empty] returns an empty board with no mines placed and no squares
    revealed *)

val custom_empty : int -> int -> t
(** [custom_empty x y] returns an empty board with dimensions [x] and
    [y] with no mines placed and no squares revealed. Requires that [x]
    and [y] are positive integers from 1 to 100. *)

val set_n_mines : t -> int -> loc -> unit
(** [set_mines board n start_loc] modifies [board] by placing [n] mines
    pseudorandomly throughout. There will not be a mine at or around the
    location denoted by [start_loc]. Requires: input does not have any
    mines already set or squares revealed, [start_loc] is a valid
    location for [board], and n is at least 0 and less than the number
    of empty tiles on the board *)

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