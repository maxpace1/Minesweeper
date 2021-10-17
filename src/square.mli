(** Representation of static place on board.

    @author Max Pace (map328), Jerry Xu (jjx6) *)

type t
(** The abstract type of value representing a place on the gameboard. *)

exception NoOperationPerformed

exception Explode

(* Operations *)

val blank : t
(** [blank] returns a placeholder square.*)

val create_square : bool -> int -> t
(** [create_square mine around] creates a square. [is_mine] represents
    if the square itself is a mine, and [around] represents the number
    of squares around it that are mines.*)

val flag : t -> t
(** [flag sq] flips the status of the flagged square i.e. a flagged [sq]
    becomes unflagged, and an unflagged [sq] becomes flagged. *)

val dig : t -> t
(** [dig sq] "digs up" a square. Attempting to dig up [sq] if it has
    been flagged or if it has been dug up before will raise
    [NoOperationPerformed]. Raises [Explode] if square was an unflagged
    mine. *)

(* Accessing data *)

val get_dug : t -> bool
(** [get_dug sq] checks if [sq] has been dug up yet. *)

val get_flag : t -> bool
(** [get_flag sq] checks if [sq] has been dug up yet. *)

val get_val : t -> int option
(** [get_val sq] gets the number of surrounding mines of [sq] and
    returns an integer 0-8 if it isn't a mine, or if [sq] has not been
    dug up yet, returns [None] *)

val test_print : t -> string
(** TODO REMOVE DEBUG ONLY pretty print mine or number *)
