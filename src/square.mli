(** Representation of static place on board. *)

type t
(** The abstract type of value representing a place on the gameboard. *)

exception AlreadyDugUp
(** Raised when attempting to dig up square that has already been dug up *)

(* Operations *)

val blank : t

val create_square : bool -> int -> t
(** [flag is_mine around] creates a square. [is_mine] represents if the
    square itself is a mine, and [around] represents the number of
    squares around it that are mines.*)

val flag : t -> t
(** [flag sq] flips the status of the flagged square i.e. a flagged [sq]
    becomes unflagged, and an unflagged [sq] becomes flagged. *)

val dig : t -> t
(** [dig sq] "digs up" a square. Requires: [sq] has not been dug before *)

(* Accessing data *)

val get_dug : t -> bool
(** [get_dug sq] checks if [sq] has been dug up yet. *)

val get_mark : t -> bool
(** [get_mark sq] checks if [sq] has been dug up yet. *)

val get_explode : t -> bool
(** [get_explode sq] returns [true] if [sq] was a mine and got dug up,
    exploding and ending the game. *)

val get_val : t -> int option
(** [get_val sq] gets the number of surrounding mines of [sq] and
    returns an integer 0-8 if it isn't a mine and has been dug up

    it isn't a mine (0-8), or undug up (-1) or a marked mine (100) If
    exploded, return 1000*)
