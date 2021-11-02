open Game
open Board

let quit msg =
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

let rec output_input_func_pair
    (instructions : string)
    (default : int * int)
    (func_operate : int * int -> 'a)
    (error_msg : string) =
  print_endline ("\n" ^ instructions);
  print_string "> ";
  match read_line () with
  | "default" -> func_operate default
  | "quit" -> quit "Quitting!"
  | p -> (
      try func_operate (parse_pair p) with
      | _ ->
          print_endline error_msg;
          output_input_func_pair instructions default func_operate
            error_msg)

let rec move (board : Board.t) =
  print_endline
    "\nEnter the location you would like to perform your move";
  print_string "> ";
  let pos = read_line () in
  let point = parse_pair pos in
  print_endline
    "\n\
     Enter the move you would like to perform at this location (dig, \
     flag, quit)";
  print_string "> ";
  match read_line () with
  | "flag" ->
      (try Board.flag board point with
      | Failure f -> print_endline f);
      Board.pp_board board;
      move board
  | "dig" ->
      (try Board.dig board point with
      | Mine ->
          Board.pp_answers board;
          quit "There was a mine in this square! You lose!"
      | Failure f -> print_endline f);
      Board.pp_board board;
      move board
  | "quit" -> quit "Quitting!"
  | _ ->
      print_endline "Invalid move";
      Board.pp_board board;
      move board

(** [start_game start_size start_loc load] takes in a gameboard size,
    starting location, load factor and kicks off the game *)
let rec start_game (size : int * int) (loc : int * int) (load : float) =
  if load > 1.0 then quit "Load factor must be less than 1.0"
  else if load < 0.0 then quit "Load factor must be greater than 0.0"
  else
    let num_mines =
      max
        (int_of_float
           ((float_of_int (fst size * snd size) *. load) -. 9.))
        0
    in
    let board = Board.set_mines size num_mines loc in
    Board.dig board loc;
    Board.pp_board board;
    move board

(** [inputs_game start_size] takes in a gameboard size and gets user
    desired factors like starting location and load factor *)
and input_game (start_size : int * int) =
  let default_pos = (fst start_size / 2, snd start_size / 2) in
  let x, y =
    output_input_func_pair
      "Please enter the location you wish to start at as x_pos y_pos, \
       or enter \"default\" for the default location.\n"
      default_pos Fun.id "Malformed input."
  in
  print_endline
    "\n\
     Please enter the ratio of mines to squares on the board or enter \
     \"default\" for the default ratio.\n";
  print_string "> ";
  let fac =
    match read_line () with
    | "default" -> 0.2
    | otherwise -> (
        try String.trim otherwise |> float_of_string with
        | _ -> quit "Malformed input. Quitting game")
  in
  start_game start_size (x, y) fac

(** [main ()] prompts for the game to play, then starts it. *)
and main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 final minesweeper project\n";

  output_input_func_pair
    "Please enter the size of board you wish to create as x_dim y_dim, \
     or enter \"default\" for the default size. Enter \"quit\" to quit \n"
    (30, 16) input_game "Malformed input."

(* Execute the game engine. *)
let () = main ()
