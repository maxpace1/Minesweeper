open OUnit2
open Game
open Square

(** square configuations *)

let create_dug_square x = create_square false x |> dig

let mine = create_square true 1

let flagged_mine = mine |> flag

let square = create_square false 2

let flagged_square = mine |> flag

let dug_square = square |> dig

let dug_square_0 = create_dug_square 0

let dug_square_3 = create_dug_square 3

let dug_square_8 = create_dug_square 8

let create_square_test
    (name : string)
    (is_mine : bool)
    (around : int)
    (expected_output : int option) : test =
  name >:: fun _ ->
  assert_equal expected_output (create_square is_mine around |> get_val)

let square_op_test
    (op : Square.t -> Square.t)
    (test : Square.t -> bool)
    (name : string)
    (square : Square.t)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (op square |> test)

let exception_test
    (func : Square.t -> Square.t)
    (name : string)
    (ex : exn)
    (square : Square.t) : test =
  name >:: fun _ -> assert_raises ex (fun () -> func square)

let exception_dig_test = exception_test dig

let exception_flag_test = exception_test flag

let flag_test = square_op_test flag get_flag

let dig_test = square_op_test dig get_dug

let test_print
    (name : string)
    (square : Square.t)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (test_print square)

let get_val_test
    (name : string)
    (square : Square.t)
    (expected_output : int option) : test =
  name >:: fun _ -> assert_equal expected_output (get_val square)

let square_tests =
  [
    ( "get_val of blank square is None" >:: fun _ ->
      assert_equal None (get_val blank) );
    create_square_test
      "get_val of undug square that is not a mine is None" false 1 None;
    create_square_test "get_val of undug square that is a mine is None"
      true 1 None;
    create_square_test
      "get_val of undug square that has mines surrounding is None" false
      8 None;
    flag_test "blank square becomes marked when flagged" blank true;
    flag_test "unflaged square becomes marked when flagged" square true;
    flag_test "flaged square becomes unmarked when flagged"
      flagged_square false;
    flag_test "unflagged mine becomes marked when flagged" mine true;
    flag_test "flagged mine becomes unmarked when flagged" flagged_mine
      false;
    exception_flag_test
      "raise NoOperationPerformed if a dug up square is flagged"
      (NoOperationPerformed
         "This square has already been dug up, you cannot flag it!")
      dug_square_0;
    exception_dig_test "raises Explode if a mine is dug up" Explode mine;
    exception_dig_test
      "raises NoOperationPerformed if a dug up square is dug up"
      (NoOperationPerformed "This square has already been dug up!")
      dug_square;
    exception_dig_test
      "raises NoOperationPerformed if a flagged non-mine square is dug \
       up"
      (NoOperationPerformed
         "This square is flagged. To dig it up, you must unflag it \
          first!")
      flagged_square;
    exception_dig_test
      "raises NoOperationPerformed if a flagged mine is dug up"
      (NoOperationPerformed
         "This square is flagged. To dig it up, you must unflag it \
          first!")
      flagged_mine;
    dig_test "blank square becomes dug when dug" blank true;
    dig_test "undug square becomes dug when dug" square true;
    get_val_test "get_val of value of blank square should be None" blank
      None;
    get_val_test "get_val of value of square not dug up should be None"
      square None;
    get_val_test "get_val of value of a mine should be None" mine None;
    get_val_test
      "get_val of value of a dug square with around 0 should be 0"
      dug_square_0 (Some 0);
    get_val_test
      "get_val of value of a dug square with around 0 should be 3"
      dug_square_3 (Some 3);
    get_val_test
      "get_val of value of a dug square with around 0 should be 8"
      dug_square_8 (Some 8);
    test_print "mine should be represented as a *" mine "*";
    test_print
      "non-mine squares should be represented with the number of mines \
       around it"
      dug_square_3 "3";
  ]

open Board

let _ = Random.init (int_of_float (Unix.gettimeofday ()))

let exc_test name f arg exc =
  name >:: fun _ -> assert_raises exc (fun () -> f arg)

let custom_empty_test
    (name : string)
    (dimx : int)
    (dimy : int)
    (expected_output : int * int) : test =
  name >:: fun _ ->
  assert_equal
    (custom_empty dimx dimy |> dim_x, custom_empty dimx dimy |> dim_y)
    expected_output

let alter_board_test
    (func : Square.t -> bool)
    (func2 : t -> loc -> unit)
    (name : string)
    (board : Board.t)
    (loc : loc)
    (expected_output : bool) : test =
  name >:: fun _ ->
  func2 board loc;
  assert_equal (get_loc_apply_fun board loc func) expected_output

let flag_test = alter_board_test get_flag flag

let dig_test = alter_board_test get_dug dig

let trial_dig square_x =
  if not (Square.get_dug square_x) then Square.dig square_x
  else square_x

let loc_value_test name board loc out =
  name >:: fun _ ->
  assert_equal out
    (match Square.get_val (get_loc_apply_fun board loc trial_dig) with
    | Some i -> i
    | None -> failwith "Nothing to see here")

let empty_board = set_mines (30, 16) 0 (0, 0)

let random_board = set_mines (30, 16) 99 (15, 8)

let _ = flag empty_board (1, 1)

let _ = dig empty_board (5, 5)

let mine_board = set_mines (10, 10) 91 (5, 5)

let generate_board =
  print_endline
    "Generating random boards to test invariants. Please wait.";
  let dim_x = Random.int 90 + 10 in
  let dim_y = Random.int 90 + 10 in
  let mines = Random.int ((dim_x * dim_y) - 9) in
  let start_loc = (Random.int dim_x, Random.int dim_y) in
  for i = 0 to 1000 do
    if i = 250 then print_endline "250/1000"
    else if i = 500 then print_endline "500/1000"
    else if i = 750 then print_endline "750/1000"
    else if i = 1000 then print_endline "Done. 1000/1000";
    set_mines (dim_x, dim_y) mines start_loc |> ignore
  done

let board_tests =
  [
    ( "custom empty should raise Failure with invalid dimensions"
    >:: fun _ ->
      assert_raises (Failure "Bad Size Arguments") (fun () ->
          custom_empty 5 100) );
    custom_empty_test "custom square board gets dimensions correctly" 10
      10 (10, 10);
    custom_empty_test "custom nonsquare board gets dimensions correctly"
      99 15 (99, 15);
    loc_value_test "empty board bottom left corner is 0" empty_board
      (0, 0) 0;
    loc_value_test "empty board bottom right corner is 0" empty_board
      (29, 0) 0;
    loc_value_test "empty board top left corner is 0" empty_board
      (0, 15) 0;
    loc_value_test "empty board top right corner is 0" empty_board
      (29, 15) 0;
    exc_test "dig mine raises Mine" (dig mine_board) (0, 0) Mine;
    exc_test "digging a dug square should raise failure"
      (dig empty_board) (5, 5)
      (Failure "This square has already been dug up!");
    exc_test "digging a flagged square should raise failure"
      (dig empty_board) (1, 1)
      (Failure
         "This square is flagged. To dig it up, you must unflag it \
          first!");
    exc_test "flagging a dug up square should raise failure"
      (flag empty_board) (5, 5)
      (Failure
         "This square has already been dug up, you cannot flag it!");
    ( "empty board should have default size of 30 by 16" >:: fun _ ->
      assert_equal (empty |> dim_x, empty |> dim_y) (30, 16) );
    flag_test "flagging unflagged square should mark it as flagged"
      empty_board (0, 0) true;
    flag_test "flagging flagged square should mark it as unflagged"
      empty_board (1, 1) false;
    dig_test "digging undug square should mark it as dug"
      (set_mines (30, 16) 0 (0, 0))
      (2, 2) true;
    loc_value_test "FF digging mine board start_pos (1 ,0) is 3"
      mine_board (6, 5) 3;
    loc_value_test "FF digging mine board start_pos (1 ,1) is 5"
      mine_board (6, 6) 5;
    loc_value_test "FF digging mine board start_pos (0 ,1) is 3"
      mine_board (5, 6) 3;
    loc_value_test "FF digging mine board start_pos (-1 ,1) is 5"
      mine_board (4, 6) 5;
    loc_value_test "FF digging mine board start_pos (-1 ,0) is 3"
      mine_board (4, 5) 3;
    loc_value_test "FF digging mine board start_pos (-1 ,-1) is 5"
      mine_board (4, 4) 5;
    loc_value_test "FF digging mine board start_pos (0 ,-1) is 3"
      mine_board (5, 4) 3;
    loc_value_test "FF digging mine board start_pos (1 ,-1) is 5"
      mine_board (6, 4) 5;
    loc_value_test "FF digging mine board start_pos is 0" mine_board
      (5, 5) 0;
    ( "test to output pretty printed empty board" >:: fun _ ->
      print_endline "\n\n";
      pp_board empty_board;
      assert true );
    ( "test to output answers pretty printed empty board" >:: fun _ ->
      print_endline "\n\n";
      pp_answers empty_board;
      assert true );
    ( "test to output pretty printed random board" >:: fun _ ->
      print_endline "\n\n";
      pp_board random_board;
      assert true );
    ( "test to output answers pretty printed random board" >:: fun _ ->
      print_endline "\n\n";
      pp_answers random_board;
      assert true );
    ( "test to output pretty printed mine board" >:: fun _ ->
      print_endline "\n\n";
      pp_board mine_board;
      assert true );
    ( "test to output answers pretty printed mine board" >:: fun _ ->
      print_endline "\n\n";
      pp_answers mine_board;
      assert true );
  ]

let suite =
  "test suite for minesweeper"
  >::: List.flatten [ square_tests; board_tests ]

let _ = run_test_tt_main suite
