open Game
open Board

let main () =
  let my_board = Board.set_mines (36, 10) 99 (18, 5) in
  dig my_board (18, 5);
  print_endline
    "\n\
     DEMO START\n\n\
     Generate an arbitrarily sized board and show what player would see\n";
  pp_board my_board;
  print_endline "\nPrint answers\n";
  pp_answers my_board;
  print_endline
    "\n\
     Dig up the squares surrounding the starting location (can't have \
     any mines on or next to the starting location.\n\
     Normally, this would auto-reveal, but our floodfill expose \
     algorithm is for MS2. ";
  dig my_board (19, 5);
  dig my_board (19, 6);
  dig my_board (18, 6);
  dig my_board (17, 6);
  dig my_board (17, 5);
  dig my_board (17, 4);
  dig my_board (18, 4);
  dig my_board (19, 4);
  pp_board my_board;

  print_endline
    "\n\n\
     Generate filled board (board with all possible locations filled \
     with mines), different sizes supported";
  let my_board = Board.set_mines (10, 10) 91 (5, 5) in
  pp_answers my_board;
  print_endline
    "\n\nWhat user sees (after same dig operations as before)";
  dig my_board (5, 5);
  dig my_board (6, 5);
  dig my_board (6, 6);
  dig my_board (5, 6);
  dig my_board (4, 6);
  dig my_board (4, 5);
  dig my_board (4, 4);
  dig my_board (5, 4);
  dig my_board (6, 4);
  pp_board my_board;
  print_endline "\nLet's flag some squares\n";
  Board.flag my_board (3, 2);
  Board.flag my_board (3, 3);
  Board.flag my_board (3, 4);
  Board.flag my_board (3, 5);
  Board.flag my_board (3, 6);
  Board.flag my_board (3, 7);
  Board.flag my_board (3, 8);
  pp_board my_board;
  print_endline "\nWe can also unflag squares\n";
  Board.flag my_board (3, 2);
  Board.flag my_board (3, 8);
  pp_board my_board;
  print_endline
    "\n\
     Attempting to dig up a square that has been flagged isn't \
     possible. Let's try (3, 5).\n";
  Board.dig my_board (3, 5);
  pp_board my_board;
  print_endline
    "\n\
     Attempting to flag a square that has already been dug up isn't \
     possible. Let's try (5, 5).\n";
  Board.flag my_board (5, 5);
  pp_board my_board;
  print_endline
    "\n\
     Attempting to dig up a square that has already been dug up isn't \
     possible. Let's try (5, 5).\n";
  Board.dig my_board (5, 5);
  pp_board my_board;
  print_endline
    "\n\
     Attempting to dig up a square that is a mine blows you up and \
     raises an Exception that in our user interactions, will end the \
     game. Let's try (2, 5).\n";
  Board.dig my_board (2, 5);
  pp_board my_board

(* Execute demo. *)
let () = main ()
