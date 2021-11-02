(** A representation of the gameboard *)

type t
(** The abstract type representing a gameboard *)

val dim_x : t -> int
(** The size of the board in the x-direction, i.e. the number of Squares
    in one row *)

val dim_y : t -> int
(** The size of the board in the y-direction, i.e. the number of Squares
    in one column *)

type loc = int * int
(** The type representation of the location of a single square *)

exception Mine
(** The exception raised when a mine is attempted to be dug *)

val get_loc_apply_fun : t -> loc -> (Square.t -> 'a) -> 'a
(** [get_loc_apply_fun board location sq_fun] gets the square at a given
    [location] from board [board] and applies a function [sq_fun] to it,
    returning the result. *)

val start_time : int
(** The time the game was started *)

(* Operations *)

val empty : t
(** [empty] returns an empty board with no mines placed and no squares
    revealed. The default size for an empty board is 30 by 16 *)

val custom_empty : int -> int -> t
(** [custom_empty x y] returns an empty board with dimensions [x] and
    [y] with no mines placed and no squares revealed. Requires that [x]
    and [y] are positive integers from 10 to 99. *)

val set_mines : loc -> int -> loc -> t
(** [set_mines dim n start_loc] modifies an empty board of dimensions
    [dim] by placing [n] mines pseudorandomly throughout. There will not
    be a mine at or around the location denoted by [start_loc].
    Requires: input does not have any mines already set or squares
    revealed, [start_loc] is a valid location for [board], and n is at
    least 0 and less than the number of empty tiles on the board minus 9
    tiles (the start location and the 8 surrounding tiles) *)

val flag : t -> loc -> unit
(** [flag board index] flips the flagged state of the square located at
    [index] in [board]. Requires: [index] is a valid location for
    [board] *)

val dig : t -> loc -> unit
(** [dig board index] digs (clears) the square located at [index] in
    [board]. Requires: [index] is a valid location for [board]. Raises
    [Mine] if the location being cleared contains a mine *)

(* Printing Data *)

val pp_board : t -> unit
(** Pretty prints a board to the console *)

val pp_answers : t -> unit
(** TODO HIDE DEBUG ONLY Pretty prints a board to the console *)
