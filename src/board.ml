type sq_type = Square.t

type t = sq_type Array.t Array.t

type loc = int * int

let _ = Random.init (int_of_float (Unix.gettimeofday ()))

exception Mine

(* AF: A 2D array of squares.

   RI: Each square has a [mines_around] field that accurately represents
   the number of mines surrounding it, and is a valid square as laid out
   in the square compilation unit. Each element in the array must be the
   same length. The board cannot be empty (i.e. contain no squares) *)

let empty = Array.make_matrix 30 16 Square.blank

let custom_empty x y =
  if 10 <= x && x <= 99 && 10 <= y && y <= 99 then
    Array.make_matrix x y Square.blank
  else failwith "Bad Size Arguments"

let dim_x b = Array.length b

let dim_y b = Array.length b.(0)

let check_loc my_board (random_loc : loc) : bool =
  0 <= fst random_loc
  && fst random_loc < dim_x my_board
  && 0 <= snd random_loc
  && snd random_loc < dim_y my_board

let get_loc my_board (random_loc : loc) =
  assert (check_loc my_board random_loc);
  my_board.(fst random_loc).(snd random_loc)

let get_loc_apply_fun
    my_board
    (random_loc : loc)
    (sq_fun : Square.t -> 'a) =
  assert (check_loc my_board random_loc);
  sq_fun (get_loc my_board random_loc)

let generate_adj_pts (my_board : t) (random_loc : loc) : loc list =
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

let rep_ok b =
  for y = 0 to dim_y b - 1 do
    for x = 0 to dim_x b - 1 do
      let each_loc = (x, y) in
      assert (check_loc b each_loc);
      assert (
        Square.ok_checker (get_loc b each_loc)
          (List.map (get_loc b) (generate_adj_pts b each_loc)))
    done
  done;
  if Array.length b <= 1 then ()
  else
    for x = 0 to dim_x b - 2 do
      assert (Array.length b.(x) = Array.length b.(x + 1))
    done

let return_rep_ok t =
  rep_ok t;
  t

let calculate_mines (my_board : t) (is_mine : bool Array.t Array.t) :
    int Array.t Array.t =
  let ret_arr = Array.make_matrix (dim_x my_board) (dim_y my_board) 0 in
  for x = 0 to dim_x my_board - 1 do
    for y = 0 to dim_y my_board - 1 do
      ret_arr.(x).(y) <-
        List.fold_left
          (fun acc (x, y) -> if is_mine.(x).(y) then acc + 1 else acc)
          0
          (generate_adj_pts my_board (x, y))
    done
  done;
  ret_arr

let copy_mines (my_board : t) (acc_mines : bool Array.t Array.t) =
  let acc_totals = calculate_mines my_board acc_mines in
  for x = 0 to dim_x my_board - 1 do
    for y = 0 to dim_y my_board - 1 do
      let each_loc = (x, y) in
      my_board.(x).(y) <-
        Square.create_square
          (get_loc acc_mines each_loc)
          (get_loc acc_totals each_loc)
    done
  done;
  my_board

let rec set_repeat_mines
    (my_board : t)
    (number_mines : int)
    (acc : bool Array.t Array.t)
    (bad_loc : loc list) =
  if number_mines = 0 then acc
  else
    let mine_loc =
      (Random.int (dim_x my_board), Random.int (dim_y my_board))
    in
    let mine_sq = acc.(fst mine_loc).(snd mine_loc) in
    if mine_sq || List.mem mine_loc bad_loc then
      set_repeat_mines my_board number_mines acc bad_loc
      (* try and randomly place a mine again if the spot was already a
         mine *)
    else (
      acc.(fst mine_loc).(snd mine_loc) <- true;
      set_repeat_mines my_board (number_mines - 1) acc bad_loc)

let set_mines my_board_dim number_mines start_loc =
  assert (
    number_mines >= 0
    && number_mines <= (fst my_board_dim * snd my_board_dim) - 9);

  let my_board = custom_empty (fst my_board_dim) (snd my_board_dim) in
  rep_ok my_board;
  if check_loc my_board start_loc then
    let off_limits = generate_adj_pts my_board start_loc in
    let mine_locs =
      set_repeat_mines my_board number_mines
        (Array.make_matrix (dim_x my_board) (dim_y my_board) false)
        (start_loc :: off_limits)
    in
    return_rep_ok (copy_mines my_board mine_locs)
  else failwith "Invalid start position"

let flag b (i : loc) =
  rep_ok b;
  b.(fst i).(snd i) <- Square.flag b.(fst i).(snd i);
  rep_ok b

let dig b i =
  rep_ok b;
  b.(fst i).(snd i) <-
    (try Square.dig b.(fst i).(snd i) with
    | Square.Explode -> raise Mine
    | Square.NoOperationPerformed -> raise Mine);
  rep_ok b

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

let pp_answers b =
  ANSITerminal.(
    for y = 0 to dim_y b - 1 do
      for x = 0 to dim_x b - 1 do
        print_string
          [ Background White; Foreground Black ]
          (if x = 0 then
           let index = dim_y b - 1 - y in
           (if index < 10 then "0" else "") ^ string_of_int index ^ "|"
          else "");
        pp_single_square_string
          (Square.test_print b.(x).(y))
          (pp_color_match (Square.test_print b.(x).(y)))
      done;
      print_string [ default ] "\n"
    done;
    add_x_axis (dim_x b);
    print_string [ default ] " \n")

let pp_board b =
  pp_answers b;
  print_endline "\n\n\n";
  ANSITerminal.(
    for y = 0 to dim_y b - 1 do
      for x = 0 to dim_x b - 1 do
        print_string
          [ Background White; Foreground Black ]
          (if x = 0 then
           let index = dim_y b - 1 - y in
           (if index < 10 then "0" else "") ^ string_of_int index ^ "|"
          else "");
        pp_given_location b (x, y)
      done;
      print_string [ default ] "\n"
    done;
    add_x_axis (dim_x b))

let to_string b = rep_ok b
