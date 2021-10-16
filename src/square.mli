(** Representation of static place on board. *)

type t
(** The abstract type of value representing a place on the gameboard. *)


(* Operations *)

val mark: t -> t
(** Flips the status of the marked mine
    I.e. a marked mine becomes unmarked, and an unmarked mine becomes marked. *)


val dig : t -> t 
(** "Digs up" a square
    This method can only be invoked once on a given square, safety checks within the game will make sure of that. *)
 
(* Accessing data *)


val get_display : t -> int 

(** Chooses image for display
    This method tells the game what image should be displayed on the square.
    This method should also be used for determining valid dig operations.
    Returns the number of mines surrounding for if it isn't a mine (0-8), or undug up (-1) or a marked mine (100) If exploded, return 1000 *)
