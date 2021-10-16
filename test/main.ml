open OUnit2
open Game
open Square

(** square configuations *)
let mine = create_square true 1

let marked_mine = mine |> flag

let square = create_square false 2

let marked_square = mine |> flag

let dug_square = square |> dig

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

let flag_test = square_op_test flag get_flag

let dig_test = square_op_test dig get_dug

let square_tests =
  [
    ( "value of blank square should be None" >:: fun _ ->
      assert_equal None (get_val blank) );
    create_square_test
      "value of square created from create_square should return None \
       for a square that is not a mine"
      false 1 None;
    create_square_test
      "value of square created from create_square should return None \
       for a square that is a mine"
      true 1 None;
    create_square_test
      "value of square created from create_square should return None \
       when there are more than 1 mine around the square"
      false 10 None;
    flag_test "blank square should become marked when flagged" blank
      true;
    flag_test "unmarked square should become marked when flagged" square
      true;
    flag_test "marked square should become unmarked when flagged"
      marked_square false;
    flag_test "unmarked mine should become marked when flagged" mine
      true;
    flag_test "marked mine should become unmarked when flagged"
      marked_mine false;
    ( "should raise Explode if a mine is dug up" >:: fun _ ->
      assert_raises Explode (fun () -> dig mine) );
    dig_test "blank sqaure should become dug when dug" blank true;
    dig_test "undug sqaure should become dug when dug" square true;
  ]

let board_tests = []

let suite =
  "test suite for minesweeper"
  >::: List.flatten [ square_tests; board_tests ]

let _ = run_test_tt_main suite
