open OUnit2
open Game
open Square

(** square configuations *)

let create_valid_square x = create_square false x |> dig

let mine = create_square true 1

let marked_mine = mine |> flag

let square = create_square false 2

let marked_square = mine |> flag

let dug_square = square |> dig

let dug_square_0 = create_valid_square 0

let dug_square_3 = create_valid_square 3

let dug_square_8 = create_valid_square 8

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

let exception_dig_test (name : string) (ex : exn) (square : Square.t) :
    test =
  name >:: fun _ -> assert_raises ex (fun () -> dig square)

let flag_test = square_op_test flag get_flag

let dig_test = square_op_test dig get_dug

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
    flag_test "unmarked square becomes marked when flagged" square true;
    flag_test "marked square becomes unmarked when flagged"
      marked_square false;
    flag_test "unmarked mine becomes marked when flagged" mine true;
    flag_test "marked mine becomes unmarked when flagged" marked_mine
      false;
    exception_dig_test "raises Explode if a mine is dug up" Explode mine;
    exception_dig_test
      "raises NoOperationPerformed if a dug up square is dug up"
      NoOperationPerformed dug_square;
    exception_dig_test
      "raises NoOperationPerformed if a flagged non-mine square is dug \
       up"
      NoOperationPerformed marked_square;
    exception_dig_test
      "raises NoOperationPerformed if a flagged mine is dug up"
      NoOperationPerformed marked_mine;
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
  ]

open Board

let exc_test name f arg exc =
  name >:: fun _ -> assert_raises exc (fun () -> f arg)

let get_square board (x, y) =
  String.get (to_string board)
    ((x * 3) + 3
    + ((dim_y board - 1 - y) * ((dim_x board * 3) + 3 + 1))
    + 1)

let loc_value_test name board loc out =
  name >:: fun _ ->
  assert_equal out (get_square board loc) ~printer:Char.escaped

let empty_board =
  let b = empty in
  set_mines b 0 (0, 0);
  b

let mine_board =
  let b = custom_empty 10 10 in
  set_mines b 91 (5, 5);
  b

let board_tests =
  [
    loc_value_test "empty board bottom left corner is 0" empty_board
      (0, 0) '0';
    loc_value_test "empty board bottom right corner is 0" empty_board
      (29, 0) '0';
    loc_value_test "empty board top left corner is 0" empty_board
      (0, 15) '0';
    loc_value_test "empty board top right corner is 0" empty_board
      (29, 15) '0';
    loc_value_test "mine board top left corner is *" mine_board (0, 0)
      '*';
    exc_test "dig mine raises Mine" (dig mine_board) (0, 0) Mine;
  ]

let suite =
  "test suite for minesweeper"
  >::: List.flatten [ square_tests; board_tests ]

let _ =
  run_test_tt_main suite;
  let my_board = Board.custom_empty 30 16 in
  Board.set_mines my_board 99 (15, 8);
  print_endline (Board.to_string mine_board)
