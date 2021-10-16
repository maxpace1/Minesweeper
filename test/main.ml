open OUnit2
open Game
open Square

(** square configuations *)
let unmarked_mine = create_square true 1

let marked_mine = unmarked_mine |> flag

let unmarked_square = create_square false 2

let marked_square = unmarked_mine |> flag

let create_square_test
    (name : string)
    (is_mine : bool)
    (around : int)
    (expected_output : int option) : test =
  name >:: fun _ ->
  assert_equal expected_output (create_square is_mine around |> get_val)

let flag_test
    (name : string)
    (square : Square.t)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (flag square |> get_mark)

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
    flag_test "unmarked square should become marked when flagged"
      unmarked_square true;
    flag_test "marked square should become unmarked when flagged"
      marked_square false;
    flag_test "unmarked mine should become marked when flagged"
      unmarked_mine true;
    flag_test "marked mine should become unmarked when flagged"
      marked_mine false;
  ]

let board_tests = []

let suite =
  "test suite for minesweeper"
  >::: List.flatten [ square_tests; board_tests ]

let _ = run_test_tt_main suite
