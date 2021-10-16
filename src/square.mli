(** Representation of static place on board. *)

type t
(** The abstract type of value representing a place on the gameboard. *)

(* Operations *)

val flag : t -> t
(** Flips the status of the flagged mine i.e. a flagged mine becomes
    unflagged, and an unflagged mine becomes flagged. *)

val dig : t -> t
(** "Digs up" a square. Requires: the square has not been dug before *)

(* Accessing data *)

val get_val : t -> int
(** Gets the number of surrounding mines of a square and returns an
    integer 0-8 *)
