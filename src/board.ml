type sq_type = Square.t

type t = sq_type Array.t Array.t

type loc = int * int

let random = Random.init (int_of_float (Unix.gettimeofday ()))

exception Mine

(* let empty = Array.make (30 * 16) Square.blank *)
let empty = Array.make_matrix 30 16 Square.blank

let custom_empty x y =
  if 10 <= x && x <= 99 && 10 <= y && y <= 99 then
    Array.make_matrix x y Square.blank
  else failwith "Bad Size Arguments"

let check_loc (my_board : t) (random_loc : loc) : bool =
  0 <= fst random_loc
  && fst random_loc < Array.length my_board
  && 0 <= snd random_loc
  && snd random_loc < Array.length my_board.(0)

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

let calculate_mines (my_board : t) (is_mine : bool Array.t Array.t) :
    int Array.t Array.t =
  let ret_arr =
    Array.make_matrix (Array.length my_board)
      (Array.length my_board.(0))
      0
  in
  for x = 0 to Array.length my_board - 1 do
    for y = 0 to Array.length my_board.(0) - 1 do
      ret_arr.(x).(y) <-
        List.fold_left
          (fun acc (x, y) ->
            (* print_endline (string_of_int x ^ " " ^ string_of_int y ^
               "\n"); *)
            if is_mine.(x).(y) then acc + 1 else acc)
          0
          (generate_adj_pts my_board (x, y))
    done
  done;
  ret_arr

let copy_mines (my_board : t) (acc_mines : bool Array.t Array.t) =
  let acc_totals = calculate_mines my_board acc_mines in
  for x = 0 to Array.length my_board - 1 do
    for y = 0 to Array.length my_board.(0) - 1 do
      my_board.(x).(y) <-
        Square.create_square acc_mines.(x).(y) acc_totals.(x).(y)
    done
  done

let rec set_repeat_mines
    (my_board : t)
    (number_mines : int)
    (acc : bool Array.t Array.t)
    (bad_loc : loc list) =
  if number_mines = 0 then acc
  else
    let mine_loc =
      ( Random.int (Array.length my_board),
        Random.int (Array.length my_board.(0)) )
    in
    let mine_sq = acc.(fst mine_loc).(snd mine_loc) in
    if mine_sq || List.mem mine_loc bad_loc then
      set_repeat_mines my_board number_mines acc bad_loc
      (* try and randomly place a mine again if the spot was already a
         mine *)
    else (
      acc.(fst mine_loc).(snd mine_loc) <- true;
      set_repeat_mines my_board (number_mines - 1) acc bad_loc)

let set_mines my_board number_mines start_loc =
  if check_loc my_board start_loc then
    let off_limits = generate_adj_pts my_board start_loc in
    let mine_locs =
      set_repeat_mines my_board number_mines
        (Array.make_matrix (Array.length my_board)
           (Array.length my_board.(0))
           false)
        (start_loc :: off_limits)
    in
    copy_mines my_board mine_locs
  else failwith "Invalid start position"

let flag b i = b.(fst i).(snd i) <- Square.flag b.(fst i).(snd i)

let dig b i =
  b.(fst i).(snd i) <-
    (try Square.dig b.(fst i).(snd i) with
    | Square.Explode -> raise Mine)

let pp_board fmt b = failwith "Unimplemented"

(* TODO REMOVE (debug purposes only) *)
let shitty_toString my_board =
  let ret_str = ref "" in
  for y = 0 to Array.length my_board.(0) - 1 do
    for x = 0 to Array.length my_board - 1 do
      let new_str =
        !ret_str ^ Square.test_print my_board.(x).(y) ^ " "
      in
      ret_str := new_str
    done;
    ret_str := !ret_str ^ "\n"
  done;
  !ret_str
