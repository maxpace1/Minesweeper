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

let calc_default_mines (start_size : int * int) =
  int_of_float
    (floor
       ((99. /. 480. *. float_of_int (fst start_size * snd start_size))
       +. 0.5))

(** [inputs_game start_size] takes in a gameboard size and gets user
    desired factors like starting location and load factor *)
let rec input_game (start_size : int * int) =
  let default_pos = (fst start_size / 2, snd start_size / 2) in
  let x, y =
    output_input_func_pair
      "Please enter the location you wish to start at as x_pos y_pos, \
       or enter \"default\" for the default location.\n"
      default_pos Fun.id "Malformed input."
  in
  let mines = read_mine_input start_size x y in
  start_game start_size (x, y) mines

(** [start_game start_size start_loc num_mines] takes in a gameboard
    size, starting location, number of mines and kicks off the game *)
and start_game (size : int * int) (loc : int * int) (num_mines : int) =
  let board = Board.set_mines size num_mines loc in
  Board.dig board loc;
  Board.pp_board board;
  move_loc board

and read_mine_input (start_size : int * int) (x : int) (y : int) =
  print_string
    "\n\
     Please enter the number of mines to be on the board or \
     \"default\" for the default number of mines. \n\
     Note that the number of mines must be at least 1 and no more than ";
  print_int ((fst start_size * snd start_size) - 9);
  print_endline ".\n";
  print_string "> ";
  match read_line () with
  | "default" -> calc_default_mines start_size
  | otherwise -> (
      try
        let input = String.trim otherwise |> int_of_string in
        if input > 0 && input <= (fst start_size * snd start_size) - 9
        then input
        else failwith "Out of bounds"
      with
      | _ ->
          print_endline "Invalid mine input!";
          read_mine_input start_size x y)

and move_loc (board : Board.t) =
  "\n>>>>>>> "
  ^ (Unix.gettimeofday () -. Board.start_time board
    |> int_of_float |> string_of_int)
  ^ " seconds elapsed <<<<<<<\n"
  |> print_endline;
  print_endline "Enter the location you would like to perform your move";
  print_string "> ";
  let pos = read_line () in
  if pos = "quit" then quit "Quitting!";
  let point =
    try parse_pair pos with
    | Failure f ->
        print_endline "\nInvalid point";
        move_loc board;
        quit ""
  in
  execute_move board pos point

and execute_move (board : Board.t) (pos : string) (point : int * int) =
  print_endline
    "\n\
     Enter the move you would like to perform at this location (dig, \
     flag, quit)\n";
  print_string "> ";
  (match read_line () with
  | "flag" -> (
      try Board.flag board point with
      | Failure f -> print_endline f
      | Invalid_argument s -> print_endline s)
  | "dig" -> (
      try Board.dig board point with
      | Mine -> lose board
      | Failure f -> print_endline f
      | Invalid_argument s -> print_endline s)
  | "quit" -> quit "Quitting!"
  | _ -> print_endline "Invalid move");
  finalize_move board

and lose (board : Board.t) =
  Board.pp_answers board;
  print_endline "There was a mine in this square! You lose!";
  print_endline "\nWould you like to play again (yes or no)?";
  print_string "> ";
  match read_line () with
  | "yes" -> main ()
  | _ -> quit "Quitting!"

and finalize_move (board : Board.t) =
  Board.pp_board board;
  if squares_left board = 0 then (
    print_endline "\nCongratulations, you found every mine!";
    print_endline "\nWould you like to play again (yes or no)?";
    print_string "> ";
    match read_line () with
    | "yes" -> main ()
    | _ -> quit "Quitting!")
  else move_loc board

(** [main ()] prompts for the game to play, then starts it. *)
and main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 final minesweeper project\n";

  output_input_func_pair
    "Please enter the size of board you wish to create as x_dim y_dim, \
     or enter \"default\" for the default size (30x16). Enter \"quit\" \
     to quit \n"
    (30, 16) input_game "Malformed input."

(* Execute the game engine. *)
let () = main ()
