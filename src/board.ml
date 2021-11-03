type sq_type = Square.t

type gameboard = sq_type Array.t Array.t

(* AF/RI for gameboard

   AF: A 2D array of squares.

   RI: Each square has a [mines_around] field that accurately represents
   the number of mines surrounding it, and is a valid square as laid out
   in the square compilation unit. Each element in the array must be the
   same length. The board cannot be empty (i.e. contain no squares) *)

type t = {
  game_board : gameboard;
  mutable squares_left : int;
  mines : int;
  mutable start_time : float;
}

type loc = int * int

let _ = Random.init (int_of_float (Unix.gettimeofday ()))

let rep_ok_on = true

exception Mine

let empty : t =
  {
    game_board = Array.make_matrix 30 16 Square.blank;
    squares_left = 480;
    mines = 0;
    start_time = Unix.gettimeofday ();
  }

let custom_empty x y : t =
  if 10 <= x && x <= 99 && 10 <= y && y <= 99 then
    {
      game_board = Array.make_matrix x y Square.blank;
      squares_left = x * y;
      mines = 0;
      start_time = Unix.gettimeofday ();
    }
  else failwith "Bad Size Arguments"

let internal_dim_x b = Array.length b

let internal_dim_y b = Array.length b.(0)

let dim_x (b : t) = internal_dim_x b.game_board

let dim_y (b : t) = internal_dim_y b.game_board

let check_loc (my_board : 'a array array) (random_loc : loc) : bool =
  0 <= fst random_loc
  && fst random_loc < internal_dim_x my_board
  && 0 <= snd random_loc
  && snd random_loc < internal_dim_y my_board

let get_loc (my_board : 'a array array) (random_loc : loc) =
  assert (check_loc my_board random_loc);
  my_board.(fst random_loc).(snd random_loc)

let get_loc_apply_fun
    my_board
    (random_loc : loc)
    (sq_fun : Square.t -> 'a) =
  assert (check_loc my_board.game_board random_loc);
  sq_fun (get_loc my_board.game_board random_loc)

let generate_adj_pts (my_board : gameboard) (random_loc : loc) :
    loc list =
  List.filter (check_loc my_board)
    [
      (1 + fst random_loc, 0 + snd random_loc);
      (1 + fst random_loc, 1 + snd random_loc);
      (0 + fst random_loc, 1 + snd random_loc);
      (-1 + fst random_loc, 1 + snd random_loc);
      (-1 + fst random_loc, 0 + snd random_loc);
      (-1 + fst random_loc, -1 + snd random_loc);
      (0 + fst random_loc, -1 + snd random_loc);
      (1 + fst random_loc, -1 + snd random_loc);
    ]

(** TODO check that squares left equals the number of squares that have
    not been dug up yet*)
let around_rep_ok (b : t) =
  if rep_ok_on then (
    assert (
      10 <= internal_dim_x b.game_board
      && internal_dim_x b.game_board <= 99
      && 10 <= internal_dim_y b.game_board
      && internal_dim_y b.game_board <= 99);
    for y = 0 to internal_dim_y b.game_board - 1 do
      for x = 0 to internal_dim_x b.game_board - 1 do
        let each_loc = (x, y) in
        assert (check_loc b.game_board each_loc);
        assert (
          Square.ok_checker
            (get_loc b.game_board each_loc)
            (List.map (get_loc b.game_board)
               (generate_adj_pts b.game_board each_loc)))
      done
    done;
    if Array.length b.game_board <= 1 then ()
    else
      for x = 0 to internal_dim_x b.game_board - 2 do
        assert (
          Array.length b.game_board.(x)
          = Array.length b.game_board.(x + 1))
      done)
  else assert true

let squares_left_rep_ok (b : t) =
  if rep_ok_on then (
    assert (
      0 <= b.squares_left
      && b.squares_left
         <= internal_dim_x b.game_board * internal_dim_y b.game_board);
    let expected_dug_left =
      (internal_dim_x b.game_board * internal_dim_y b.game_board)
      - b.squares_left - b.mines
    in
    assert (
      Array.(b.game_board |> to_list |> concat |> to_list)
      |> List.filter Square.get_dug
      |> List.length = expected_dug_left))
  else assert true

let rep_ok (b : t) =
  if rep_ok_on then (
    around_rep_ok b;
    squares_left_rep_ok b)

let return_rep_ok t =
  rep_ok t;
  t

let calculate_mines
    (my_board : gameboard)
    (is_mine : bool Array.t Array.t) : int Array.t Array.t =
  let ret_arr =
    Array.make_matrix
      (internal_dim_x my_board)
      (internal_dim_y my_board)
      0
  in
  for x = 0 to internal_dim_x my_board - 1 do
    for y = 0 to internal_dim_y my_board - 1 do
      ret_arr.(x).(y) <-
        List.fold_left
          (fun acc (x, y) -> if is_mine.(x).(y) then acc + 1 else acc)
          0
          (generate_adj_pts my_board (x, y))
    done
  done;
  ret_arr

let copy_mines (my_board : gameboard) (acc_mines : bool Array.t Array.t)
    =
  let acc_totals = calculate_mines my_board acc_mines in
  for x = 0 to internal_dim_x my_board - 1 do
    for y = 0 to internal_dim_y my_board - 1 do
      let each_loc = (x, y) in
      my_board.(x).(y) <-
        Square.create_square
          (get_loc acc_mines each_loc)
          (get_loc acc_totals each_loc)
    done
  done;
  my_board

let rec set_repeat_mines
    (my_board : gameboard)
    (number_mines : int)
    (acc : bool Array.t Array.t)
    (bad_loc : loc list) =
  if number_mines = 0 then acc
  else
    let mine_loc =
      ( Random.int (internal_dim_x my_board),
        Random.int (internal_dim_y my_board) )
    in
    let mine_sq = acc.(fst mine_loc).(snd mine_loc) in
    if mine_sq || List.mem mine_loc bad_loc then
      set_repeat_mines my_board number_mines acc bad_loc
      (* try and randomly place a mine again if the spot was already a
         mine *)
    else (
      acc.(fst mine_loc).(snd mine_loc) <- true;
      set_repeat_mines my_board (number_mines - 1) acc bad_loc)

let set_mines my_board_dim number_mines start_loc : t =
  if
    10 <= fst my_board_dim
    && fst my_board_dim <= 99
    && 10 <= snd my_board_dim
    && snd my_board_dim <= 99
  then (
    assert (
      number_mines >= 0
      && number_mines <= (fst my_board_dim * snd my_board_dim) - 9);

    let my_board = custom_empty (fst my_board_dim) (snd my_board_dim) in
    rep_ok my_board;
    if check_loc my_board.game_board start_loc then
      let off_limits = generate_adj_pts my_board.game_board start_loc in
      let mine_locs =
        set_repeat_mines my_board.game_board number_mines
          (Array.make_matrix
             (internal_dim_x my_board.game_board)
             (internal_dim_y my_board.game_board)
             false)
          (start_loc :: off_limits)
      in
      return_rep_ok
        {
          game_board = copy_mines my_board.game_board mine_locs;
          squares_left =
            (fst my_board_dim * snd my_board_dim) - number_mines;
          mines = number_mines;
          start_time = Unix.gettimeofday ();
        }
    else failwith "Invalid start position")
  else failwith "Bad Size Arguments"

let flag (b : t) (i : loc) =
  rep_ok b;
  b.game_board.(fst i).(snd i) <-
    (try Square.flag b.game_board.(fst i).(snd i) with
    | Square.NoOperationPerformed s -> failwith s);
  rep_ok b

let ok_dig_around b (i : loc) =
  rep_ok b;
  assert (check_loc b.game_board i);
  let sq = get_loc b.game_board i in
  match Square.get_val sq with
  | None -> false
  | Some int_val ->
      int_val
      = (generate_adj_pts b.game_board i
        |> List.map (get_loc b.game_board)
        |> List.filter Square.get_flag
        |> List.length)

let rec dig (b : t) (i : loc) =
  rep_ok b;
  if not (check_loc b.game_board i) then
    raise (Invalid_argument "Invalid location on the board!")
  else
    let sq = get_loc b.game_board i in
    b.game_board.(fst i).(snd i) <-
      (try Square.dig sq with
      | Square.Explode -> raise Mine
      | Square.NoOperationPerformed s -> failwith s);
    b.squares_left <- b.squares_left - 1;
    rep_ok b;
    dig_around b i;
    rep_ok b

and has_dug b i = get_loc b i |> Square.get_dug |> not

and dig_around b i =
  if ok_dig_around b i then
    ignore
      (generate_adj_pts b.game_board i
      |> List.filter (check_loc b.game_board)
      |> List.filter (has_dug b.game_board)
      |> List.map (fun my_loc ->
             if has_dug b.game_board my_loc then dig b my_loc else ()))
  else ()

let add_x_axis n =
  ANSITerminal.(
    let style = [ Background White; Foreground Black ] in
    print_string style "  +";
    for x = 0 to n - 1 do
      print_string style "---"
    done;
    print_string [ default ] "\n";
    print_string style "   ";
    for x = 0 to n - 1 do
      print_string style
        ((if x < 10 then "0" else "") ^ string_of_int x ^ " ")
    done;
    print_string [ default ] "\n")

let pp_color_match process_char =
  ANSITerminal.(
    match process_char with
    | "0" -> [ Background White ]
    | "1" -> [ Background White; Foreground Blue ]
    | "2" -> [ Background White; Foreground Green ]
    | "3" -> [ Background White; Foreground Red ]
    | "4" -> [ Background White; Foreground Cyan ]
    | "5" -> [ Background White; Foreground Black ]
    | "6" -> [ Background White; Foreground Magenta ]
    | "7" -> [ Background White; Foreground Yellow ]
    | "8" -> [ Background Black; Foreground White ]
    | _ -> [ Background Green ])

let pp_single_square_string process_char my_style =
  ANSITerminal.(
    let output_chars =
      if process_char = "🚩" then " " ^ process_char
      else if process_char <> "0" then " " ^ process_char ^ " "
      else "   "
    in
    print_string (Bold :: my_style) output_chars)

let pp_given_location b loc =
  let given_sq = b.(fst loc).(snd loc) in
  let out_char = Square.get_val given_sq in
  let process_char =
    match out_char with
    | Some i -> string_of_int i
    | None -> if Square.get_flag given_sq then "🚩" else " "
  in
  pp_single_square_string process_char (pp_color_match process_char)

let pp_answers (b : t) =
  ANSITerminal.(
    for y = 0 to internal_dim_y b.game_board - 1 do
      for x = 0 to internal_dim_x b.game_board - 1 do
        print_string
          [ Background White; Foreground Black ]
          (if x = 0 then
           let index = internal_dim_y b.game_board - y - 1 in
           (if index < 10 then "0" else "") ^ string_of_int index ^ "|"
          else "");
        pp_single_square_string
          (Square.test_print
             (get_loc b.game_board
                (x, internal_dim_y b.game_board - y - 1)))
          (pp_color_match
             (Square.test_print
                (get_loc b.game_board
                   (x, internal_dim_y b.game_board - y - 1))))
      done;
      print_string [ default ] "\n"
    done;
    add_x_axis (internal_dim_x b.game_board);
    print_string [ default ] " \n")

let pp_board (b : t) =
  print_endline "\n";
  ANSITerminal.(
    for y = 0 to internal_dim_y b.game_board - 1 do
      for x = 0 to internal_dim_x b.game_board - 1 do
        print_string
          [ Background White; Foreground Black ]
          (if x = 0 then
           let index = internal_dim_y b.game_board - y - 1 in
           (if index < 10 then "0" else "") ^ string_of_int index ^ "|"
          else "");
        pp_given_location b.game_board
          (x, internal_dim_y b.game_board - y - 1)
      done;
      print_string [ default ] "\n"
    done;
    add_x_axis (internal_dim_x b.game_board));
  print_endline (string_of_int b.squares_left)

let start_time board = board.start_time
