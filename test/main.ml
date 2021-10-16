open OUnit2
open Game
open Square
open Board

let create_square_test
    (name : string)
    (is_mine : bool)
    (around : int)
    (expected_output : int option) : test =
  name >:: fun _ ->
  assert_equal expected_output (create_square is_mine around |> get_val)

let square_tests =
  [
    ( "value of blank square should be None" >:: fun _ ->
      assert_equal None (get_val blank) );
    create_square_test
      "value of square created from create_square should return None \
       for all inputs"
      false 1 None;
    create_square_test
      "value of square created from create_square should return None \
       for all inputs"
      true 1 None;
    create_square_test
      "value of square created from create_square should return None \
       for all inputs"
      true 10 None;
    create_square_test
      "value of square created from create_square should return None \
       for all inputs"
      false 10 None;
  ]

let board_tests = []

let suite =
  "test suite for minesweeper"
  >::: List.flatten [ square_tests; board_tests ]

let _ = run_test_tt_main suite
