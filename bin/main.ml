open Game
open Board

let quit_game msg =
  print_endline msg;
  Stdlib.exit 0

(** [parse_pair str] is a helper takes in a string and returns a pair of
    ints*)
let parse_pair (str : string) : int * int =
  let lst_str = String.split_on_char ' ' (String.trim str) in
  let lst_int = List.map int_of_string lst_str in
  assert (List.length lst_int = 2);
  match lst_int with
  | [ x; y ] -> (x, y)
  | _ -> failwith "Invalid input"

let output_input_func_pair
    (instructions : string)
    (default_size : int * int)
    (func_operate : int * int -> 'a)
    (error_msg : string)
    (quit_game : bool) =
  print_endline instructions;
  print_string "> ";
  match read_line () with
  | exception End_of_file ->
      print_endline error_msg;
      if quit_game then Stdlib.exit 0 else func_operate default_size
  | "default" -> func_operate default_size
  | "quit" ->
      print_endline "Quitting!";
      Stdlib.exit 0
  | otherwise -> (
      try func_operate (parse_pair otherwise) with
      | _ ->
          print_endline error_msg;
          if quit_game then Stdlib.exit 0 else func_operate default_size
      )

(** [start_game start_size start_loc load] takes in a gameboard size,
    starting location, load factor and kicks off the game *)
let rec start_game (size : int * int) (loc : int * int) (load : float) =
  if load > 1.0 then quit_game "Load factor must be less than 1.0"
  else if load < 0.0 then
    quit_game "Load factor must be greater than 0.0"
  else
    let num_mines =
      max
        (int_of_float
           ((float_of_int (fst size * snd size) *. load) -. 9.))
        0
    in
    let board = Board.set_mines size num_mines loc in
    Board.dig board loc;
    Board.pp_board board

(** [inputs_game start_size] takes in a gameboard size and gets user
    desired factors like starting location and load factor *)
and input_game (start_size : int * int) =
  let default_pos = (fst start_size / 2, snd start_size / 2) in
  let x, y =
    output_input_func_pair
      "Please enter the location you wish to start at as x_pos y_pos, \
       or enter \"default\" for the default location.\n"
      default_pos Fun.id "Malformed input. Quitting game" true
  in
  print_endline
    "Please enter the ratio of mines to squares on the board or enter \
     \"default\" for the default ratio.";
  print_string "> ";
  let fac =
    match read_line () with
    | exception End_of_file ->
        quit_game "Malformed input. Quitting game"
    | "default" -> 0.2
    | otherwise -> (
        try
          let fac_str = String.trim otherwise in
          float_of_string fac_str
        with
        | _ -> quit_game "Malformed input. Quitting game")
  in
  start_game start_size (x, y) fac

(** [main ()] prompts for the game to play, then starts it. *)
and main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 final minesweeper project\n";

  output_input_func_pair
    "Please enter the size of board you wish to create as x_dim y_dim, \
     or enter \"default\" for the default size. Enter \"default\" to \
     quit \n"
    (30, 16) input_game "Malformed input. Quitting game" true

(* Execute the game engine. *)
let () = main ()
