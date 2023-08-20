[@@@ocamlformat "disable = true"]
(** Test Plan:

    Overall, our testing approach consists of both writing OUnit Test
    Suites for compilation units command and battleship_game, and
    manually testing functions in main.ml and grid compilation unit.

    We tested functions in main.ml manually by typing "make play"
    in the terminal, interacting with the game, and observing if the
    program exhibits the expected behaviors or not. We tested all
    possible commands and some inputs of wrong command formats: for invalid 
    command, we expected the program to print out messages to tell us why the
    command is invalid and reprompt us for a new command; for right command, 
    we expected the program to print out some instructions related to the next 
    step of the game. We have manually tested our game program thoroughly enough 
    that we are confident that all functions in main.ml works in the intended 
    way.

    The parse function in command.ml was tested by OUnit Test Suites. Test cases 
    were developed through glass-box testing. We wrote two
    OUnit functions to test parse: parse_test and invalid_format_test.
    Parse_test checks that parse correctly turns a valid input string
    into the desired command output we want. Invalid_format_test checks
    that the parse function throws an invalid_format error whenever it
    encounters an invalid input string. This ensures the correctness of
    parse as we check representative outputs that parse returns. The
    other helper functions in command.ml are not being tested directly,
    but since parse calls all of them, we also ensure the correctness of
    the helper functions when all test cases of parse_test and
    invalid_format_test pass.

    Grid.ml was manually tested because directly printing the grid
    allowed us to easily visualize the resulting grid from our function
    applications.

    In battleship_game.ml, we manually tested get_mode, get_ship_1, get_ship_2,
    bomb_lst_1, bomb_lst_2, max_ship, max_bomb, and total_grid_occupied because
    they are simply getters returning a field in the state, so we did not feel
    it was necessary to test as the results are trivial. For similar reasons,
    loc_to_str, grid_to_str, and row_to_str were also tested manually as they
    are the string representations of field values so manually testing was
    sufficient. The functions print_grid, add_ship, add_bomb, add_ship_vertical,
    and add_ship_horizontal were also manually tested as their output is of type
    unit. Using make play was the most logical way to visualize the output of
    the function. By running make play we were able to ensure that the functions
    created the intended effect. The function make_state was also manually
    tested as it was utilized only for test.ml and was simply a state
    representation of the inputs which were all fields. Finally, 
    check_overlapped is implicitly tested by Ounit as it is a helper to 
    collision_detect.

    In battleship_game.ml, we tested init_state through glass-box testing since
    we accounted for every possible mode in the game. Similarly, we tested
    get_ship_type with glass-box testing by accounting for every possible ship
    type and also an input that wasn't the string representation of any ship to
    ensure the wildcard pattern returned a default ship. Also ship_size was
    tested through the glass-box method as every possible ship type was 
    accounted for. Testing energy was also done through glass-box as a change 
    in both players' energy was tested with varying initial and final amounts. 
    Also glass-box was hange_energy which also accounted for both players.
    add_ship_rep was also tested through glass-box as both players and both
    orientations were accounted for. add_ship_in_state was also glass-box with
    all types of ships tested and both players tested. add_bomb_in_state was
    also glass-box and tested both players. ship_in_bounds was also glass-box as
    we accounted for vertical, horizontal, and neither position, as well as if
    the remaining conditions (location placed and size of the ship type) made
    the out_of_bounds condition both true and false. make_ship_and_check_win is
    also glass-box as we account for all ship types, orientations, and all
    conditions that would result in a winning condition for both players.
    For the OUnit tests, we felt utilizing strictly glass-box would be the best
    means of testing since we can ensure that each part of our code is executing
    as necessary (match-statements, if-statements), reinforcing the correctness
    of our functions.

    AUTOMATICALLY TESTED BY OUNIT:
    init_state;
    get_ship_type;
    ship_size;
    energy;
    change_energy;
    add_energy;
    add_ship_rep;
    add_ship_in_state;
    add_bomb_in_state;
    make_grid;
    ship_in_bounds;
    collision_detect;
    make_ship_and_check_win;

    MANUALLY TESTED:
      getters:
        get_mode
        get_ship_1
        get_ship_2
        bomb_lst_1
        bomb_lst_2
        max_ship
        max_bomb
        total_grid_occupied
      to_str functions:
        loc_to_str
        grid_to_str
        row_to_str
      unit functions:
        print_grid
        add_ship
        add_bomb
        add_ship_vertical
        add_ship_horizontal
      helpers:
        make_state
        check_overlapped *) 

open OUnit2
open Battleship
open Battleship_game
open Command
open Grid

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [parse_test name str expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [parse str]. *)
let parse_test
    (name : string)
    (str : string)
    (expected_output : Command.command) : test =
  name >:: fun _ -> assert_equal expected_output (parse str)

(** [invalid_format_test name str ] constructs an OUnit test named
    [name] that asserts parse [str] raises an [Invalid_Format] error. *)
let invalid_format_test (name : string) (str : string) : test =
  name >:: fun _ -> assert_raises Invalid_Format (fun () -> parse str)

let parse_tests =
  [
    parse_test "Parsing the input string 'easy' yields Easy" "easy" Easy;
    parse_test "Parsing the input string 'medium' yields Medium"
      "medium" Medium;
    parse_test "Parsing the input string 'hard' yields Hard" "hard" Hard;
    parse_test "Parsing the input string 'easy' yields Easy" "   easy"
      Easy;
    parse_test "Parsing the input string 'medium' yields Medium"
      "     medium   " Medium;
    parse_test "Parsing the input string 'hard' yields Hard" "hard    "
      Hard;
    parse_test "Parsing the input string 'end' yields End_Turn" "end"
      End_Turn;
    parse_test "Parsing the input string 'energy' yields Energy"
      "energy" Energy;
    parse_test "Parsing the input string 'quit' yields Quit" "quit" Quit;
    parse_test "Parsing the input string '  end   ' yields End_Turn"
      "  end   " End_Turn;
    parse_test "Parsing the input string 'energy    ' yields Energy"
      "energy    " Energy;
    parse_test "Parsing the input string '  quit' yields Quit" "  quit"
      Quit;
    parse_test
      "Parsing the input string 'carrier 1 1 h' yields (Place_Ship \
       ((1, 1), 'carrier', 'h'))"
      "carrier 1 1 h"
      (Place_Ship ((1, 1), "carrier", "h"));
    parse_test
      "Parsing the input string 'carrier 6 7 v' yields (Place_Ship \
       ((6, 7), 'carrier', 'v'))"
      "carrier 6 7 v"
      (Place_Ship ((6, 7), "carrier", "v"));
    parse_test
      "Parsing the input string 'battleship 1 1 h' yields (Place_Ship \
       ((1, 1), 'battleship', 'h'))"
      "battleship 1 1 h"
      (Place_Ship ((1, 1), "battleship", "h"));
    parse_test
      "Parsing the input string 'battleship 6 7 v' yields (Place_Ship \
       ((6, 7), 'battleship', 'v'))"
      "battleship 6 7 v"
      (Place_Ship ((6, 7), "battleship", "v"));
    parse_test
      "Parsing the input string 'submarine 1 1 h' yields (Place_Ship \
       ((1, 1), 'submarine', 'h'))"
      "submarine 1 1 h"
      (Place_Ship ((1, 1), "submarine", "h"));
    parse_test
      "Parsing the input string 'submarine 6 7 v' yields (Place_Ship \
       ((6, 7), 'submarine', 'v'))"
      "submarine 6 7 v"
      (Place_Ship ((6, 7), "submarine", "v"));
    parse_test
      "Parsing the input string 'destroyer 1 1 h' yields (Place_Ship \
       ((1, 1), 'destroyer', 'h'))"
      "destroyer 1 1 h"
      (Place_Ship ((1, 1), "destroyer", "h"));
    parse_test
      "Parsing the input string 'destroyer 6 7 v' yields (Place_Ship \
       ((6, 7), 'destroyer', 'v'))"
      "destroyer 6 7 v"
      (Place_Ship ((6, 7), "destroyer", "v"));
    parse_test
      "Parsing the input string 'bomb_diagonal' yields Bomb_diagonal"
      "bomb_diagonal" Bomb_diagonal;
    parse_test
      "Parsing the input string '   bomb_diagonal   ' yields \
       Bomb_diagonal"
      "   bomb_diagonal   " Bomb_diagonal;
    parse_test
      "Parsing the input string 'bomb 1 1' yields (Bomb (1, 1))"
      "bomb 1 1"
      (Bomb (1, 1));
    parse_test
      "Parsing the input string 'bomb_col 2' yields (Bomb_col 2)"
      "bomb_col 2" (Bomb_col 2);
    parse_test
      "Parsing the input string 'bomb_row 3' yields (Bomb_row 3)"
      "bomb_row 3" (Bomb_row 3);
    parse_test
      "Parsing the input string ' bomb 9 7 ' yields (Bomb (9, 7))"
      " bomb 9 7 "
      (Bomb (9, 7));
    parse_test
      "Parsing the input string 'bomb_col 4 ' yields (Bomb_col 2)"
      "bomb_col 4 " (Bomb_col 4);
    parse_test
      "Parsing the input string ' bomb_row 5' yields (Bomb_row 3)"
      " bomb_row 5" (Bomb_row 5);
    parse_test
      "Parsing the input string 'bomb_cross 5 5' yields (Bomb_cross 5 \
       5)"
      "bomb_cross 5 5"
      (Bomb_cross (5, 5));
    parse_test
      "Parsing the input string '   bomb_cross     5    5   ' yields \
       (Bomb_cross 5 5)"
      "   bomb_cross     5    5   "
      (Bomb_cross (5, 5));
    parse_test
      "Parsing the input string 'bomb_square 5 5' yields (Bomb_square \
       5 5)"
      "bomb_square 5 5"
      (Bomb_square (5, 5));
    parse_test
      "Parsing the input string '   bomb_square     5    5   ' yields \
       (Bomb_square 5 5)"
      "   bomb_square     5    5   "
      (Bomb_square (5, 5));
    parse_test
      "Parsing the input string 'bomb_diagonal 9 9' yields \
       (Bomb_diagonal)"
      " bomb_diagonal " Bomb_diagonal;
    parse_test
      "Parsing the input string '  bomb_diagonal  ' yields \
       (Bomb_diagonal)"
      "  bomb_diagonal  " Bomb_diagonal;
  ]

let invalid_format_tests =
  [
    invalid_format_test
      "Parsing the input string 'easy, ' yields invalid_format \
       exception"
      "easy, ";
    invalid_format_test
      "Parsing the input string 'easy 10' yields invalid_format \
       exception"
      "easy 10";
    invalid_format_test
      "Parsing the input string 'medium 10' yields invalid_format \
       exception"
      "medium 10";
    invalid_format_test
      "Parsing the input string 'hard 9 9' raises invalid_format \
       exception"
      "hard 9 9";
    invalid_format_test
      "Parsing the input string 'end, ' yields invalid_format exception"
      "end, ";
    invalid_format_test
      "Parsing the input string 'end_turn' yields invalid_format \
       exception"
      "end_turn";
    invalid_format_test
      "Parsing the input string 'end-turn' yields invalid_format \
       exception"
      "end-turn";
    invalid_format_test
      "Parsing the input string 'end turn' yields invalid_format \
       exception"
      "end turn";
    invalid_format_test
      "Parsing the input string 'energy 10' yields invalid_format \
       exception"
      "energy 10";
    invalid_format_test
      "Parsing the input string 'I quit' raises invalid_format \
       exception"
      "I quit";
    invalid_format_test
      "Parsing the input string 'Bomb_diagonal' raises invalid_format \
       exception"
      "Bomb_diagonal";
    invalid_format_test
      "Parsing the input string 'bomb diagonal' raises invalid_format \
       exception"
      "bomb diagonal";
    invalid_format_test
      "Parsing the input string 'Carrier 1 1 h' raises invalid_format \
       exception"
      "Carrier 1 1 h";
    invalid_format_test
      "Parsing the input string 'submarine 3 3' raises invalid_format \
       exception"
      "submarine 3 3";
    invalid_format_test
      "Parsing the input string 'battleship 3 v' raises invalid_format \
       exception"
      "battleship 3 v";
    invalid_format_test
      "Parsing the input string 'destroyer 5 5 V' raises \
       invalid_format exception"
      "destroyer 5 5 V";
    invalid_format_test
      "Parsing the input string 'destroyer' raises invalid_format \
       exception"
      "destroyer";
    invalid_format_test
      "Parsing the input string 'carrier h' raises invalid_format \
       exception"
      "carrier h";
    invalid_format_test
      "Parsing the input string 'Place_ship ((1,1), carrier, h)' \
       raises invalid_format exception"
      "Place_ship ((1,1), carrier, h)";
    invalid_format_test
      "Parsing the input string 'submarine 6 7 h v' raises \
       invalid_format exception"
      "submarine 6 7 h v";
    invalid_format_test
      "Parsing the input string 'bomb 6 7 h v' raises invalid_format \
       exception"
      "bomb 6 7 h v";
    invalid_format_test
      "Parsing the input string 'bomb 6' raises invalid_format \
       exception"
      "bomb 6";
    invalid_format_test
      "Parsing the input string 'bomb 6 6 6' raises invalid_format \
       exception"
      "bomb 6 6 6";
    invalid_format_test
      "Parsing the input string 'bomb 5,5' raises invalid_format \
       exception"
      "bomb 5,5";
    invalid_format_test
      "Parsing the input string 'bomb 5 and 5' raises invalid_format \
       exception"
      "bomb 5 and 5";
    invalid_format_test
      "Parsing the input string 'bomb center' raises invalid_format \
       exception"
      "bomb center";
    invalid_format_test
      "Parsing the input string 'bomb_col 4 5' raises invalid_format \
       exception"
      "bomb_col 4 5";
    invalid_format_test
      "Parsing the input string 'bomb_col a' raises invalid_format \
       exception"
      "bomb_col a";
    invalid_format_test
      "Parsing the input string 'bomb_row 3 3' raises invalid_format \
       exception"
      "bomb_row 3 3";
    invalid_format_test
      "Parsing the input string 'bomb_row a' raises invalid_format \
       exception"
      "bomb_row a";
    invalid_format_test
      "Parsing the input string 'bomb_row 9, bomb_row 8' raises \
       invalid_format exception"
      "bomb_row 9, bomb_row 8";
    invalid_format_test
      "Parsing the input string 'bomb_col 9, bomb_col 8' raises \
       invalid_format exception"
      "bomb_col 9, bomb_col 8";
    invalid_format_test
      "Parsing the input string 'Bomb_col 5' raises invalid_format \
       exception"
      "Bomb_col 5";
    invalid_format_test
      "Parsing the input string 'Bomb_row 3' raises invalid_format \
       exception"
      "Bomb_row 3";
    invalid_format_test
      "Parsing the input string 'Bomb_cross 3' raises invalid_format \
       exception"
      "Bomb_cross 3";
    invalid_format_test
      "Parsing the input string '   Bomb_cross 3 3 h' raises \
       invalid_format exception"
      "   Bomb_cross 3 3 h";
    invalid_format_test
      "Parsing the input string '  Bomb_cross ' raises invalid_format \
       exception"
      "  Bomb_cross ";
    invalid_format_test
      "Parsing the input string 'Bomb-cross 3 3' raises invalid_format \
       exception"
      "Bomb-cross 3 3";
    invalid_format_test
      "Parsing the input string '  Bomb   cross   7   7   ' raises \
       invalid_format exception"
      "  Bomb   cross   7   7   ";
    invalid_format_test
      "Parsing the input string 'Bomb_square 3' raises invalid_format \
       exception"
      "Bomb_square 3";
    invalid_format_test
      "Parsing the input string '   Bomb_square 3 3 h' raises \
       invalid_format exception"
      "   Bomb_square 3 3 h";
    invalid_format_test
      "Parsing the input string '  Bomb_square ' raises invalid_format \
       exception"
      "  Bomb_square ";
    invalid_format_test
      "Parsing the input string 'Bomb-square 3 3' raises \
       invalid_format exception"
      "Bomb-square 3 3";
    invalid_format_test
      "Parsing the input string '  Bomb   square   7   7   ' raises \
       invalid_format exception"
      "  Bomb   square   7   7   ";
    invalid_format_test
      "Parsing the input string 'Bomb_diagonal 4 4 ' raises \
       invalid_format exception"
      "Bomb_diagonal 4 4 ";
    invalid_format_test
      "Parsing the input string '   Bomb_diagonal 4 4 h' raises \
       invalid_format exception"
      "   Bomb_diagonal 4 4 h";
    invalid_format_test
      "Parsing the input string '  Bomb_diagonal 5 ' raises \
       invalid_format exception"
      "  Bomb_diagonal 5 ";
    invalid_format_test
      "Parsing the input string 'Bomb-diagonal ' raises invalid_format \
       exception"
      "Bomb-diagonal ";
    invalid_format_test
      "Parsing the input string '  Bomb   diagonal   ' raises \
       invalid_format exception"
      "  Bomb   diagonal   ";
  ]

let command_tests = parse_tests @ invalid_format_tests

(** [init_state_test name input expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [input]. *)
let init_state_test (name : string) input expected_out : test =
  name >:: fun _ -> assert_equal input expected_out

(** [get_ship_type_test name t_str expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output]
    with [get_ship_type t_str]. *)
let get_ship_type_test
    (name : string)
    (t_str : string)
    (expected_out : ship_type) : test =
  name >:: fun _ -> assert_equal (get_ship_type t_str) expected_out

(** [ship_size_test name s_type expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output] and
    [ship_size s_type]. *)
let ship_size_test
    (name : string)
    (s_type : ship_type)
    (expected_out : int) : test =
  name >:: fun _ -> assert_equal (ship_size s_type) expected_out

(** [energy_test name player_num st expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output] and
    [energy player_num st]. *)
let energy_test
    (name : string)
    (player_num : int)
    (st : t)
    (expected_out : int) : test =
  name >:: fun _ -> assert_equal (energy player_num st) expected_out

(** [change_energy_test name player_num st amt expected_output] adds
    [amt] to [player_num]'s energy count in [st] and constructs an OUnit
    test named [name] that asserts the equality of [expected_output] and
    [energy player_num st]. *)
let change_energy_test
    (name : string)
    (player_num : int)
    (st : t)
    (amt : int)
    (expected_out : int) =
  name >:: fun _ ->
  change_energy player_num st amt;
  assert_equal (Battleship_game.energy player_num st) expected_out

(** [add_energy_test name player_num st amt expected_output] constructs
    an OUnit test named [name] that asserts the equality of
    [expected_output] and [add_energy amt player_num st]. *)
let add_energy_test
    (name : string)
    (player_num : int)
    (st : t)
    (amt : int)
    (expected_out : int) : test =
  name >:: fun _ ->
  assert_equal (add_energy amt player_num st) expected_out

(** [add_ship_rep_test name ship_acc start_pos size orientation loc_added_acc 
    expected_output] constructs an OUnit test named [name] that asserts the 
    equality of [expected_output] and [add_ship_rep ship_acc start_pos size 
    orientation loc_added_acc]. *)
let add_ship_rep_test
    (name : string)
    (ship_acc : (int * int) list)
    (start_pos : int * int)
    (size : int)
    (orientation : string)
    (loc_added_acc : int)
    (expected_out : (int * int) list) : test =
  name >:: fun _ ->
  assert_equal
    (add_ship_rep ship_acc start_pos size orientation loc_added_acc)
    expected_out

(** [add_ship_in_state_test name st start_pos player ship_type orientation 
    expected_output] constructs an OUnit test named [name] that asserts the 
    equality of [expected_output] and [add_ship_in_state st start_pos plyaer 
    ship_type orientation]. *)
let add_ship_in_state_test
    (name : string)
    (st : t)
    (start_pos : int * int)
    (player : int)
    (ship_type : string)
    (orientation : string)
    (expected_out : t) : test =
  name >:: fun _ ->
  assert_equal
    (add_ship_in_state st start_pos player ship_type orientation)
    expected_out

(** [add_bomb_in_state_test name st added_bomb player remove_energy_amt 
expected_output]
    constructs an OUnit test named [name] that asserts the equality of
    [expected_output] and
    [add_bomb_in_state st added_bomb player remove_energy_amt]. *)
let add_bomb_in_state_test
    (name : string)
    (st : t)
    (added_bomb : (int * int) list)
    (player : int)
    (remove_energy_amt : int)
    (expected_out : t) : test =
  name >:: fun _ ->
  assert_equal
    (add_bomb_in_state st added_bomb player remove_energy_amt)
    expected_out

(** [make_grid_test name rows cols expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output] and
    [make_grid rows cols]. *)
let make_grid_test
    (name : string)
    (rows : int)
    (cols : int)
    (expected_out : Grid.location array array) : test =
  name >:: fun _ -> assert_equal (make_grid rows cols) expected_out

(** [ship_in_bounds_test name row col grd typ orientation expected_output]
    constructs an OUnit test named [name] that asserts the equality of
    [expected_output] and [ship_in_bounds row col grd typ orientation]. *)
let ship_in_bounds_test
    (name : string)
    (row : int)
    (col : int)
    (grd : Grid.location array array)
    (typ : string)
    (orientation : string)
    (expected_out : bool) : test =
  name >:: fun _ ->
  assert_equal (ship_in_bounds row col grd typ orientation) expected_out

(** [collision_detect_test name shp st player expected_output]
    constructs an OUnit test named [name] that asserts the equality of
    [expected_output] and [collision_detect shp st player]. *)
let collision_detect_test
    (name : string)
    (shp : (int * int) list)
    (st : t)
    (player : int)
    (expected_out : bool) : test =
  name >:: fun _ ->
  assert_equal (collision_detect shp st player) expected_out

let st_easy = init_state "easy"
let st_med = init_state "medium"
let st_hard = init_state "hard"

let init_state_tests =
  [
    init_state_test "easy mode has max ship of 3" (max_ship st_easy) 3;
    init_state_test "easy mode has max bomb of 3" (max_bomb st_easy) 3;
    init_state_test
      "ships list is empty for player 1 initially in easy mode"
      (get_ship_1 st_easy) [];
    init_state_test
      "ships list is empty for player 2 initially in easy mode"
      (get_ship_2 st_easy) [];
    init_state_test
      "bombs list is empty for player 1 initially in easy mode"
      (bomb_lst_1 st_easy) [];
    init_state_test
      "bombs list is empty for player 2 initially in easy mode"
      (bomb_lst_2 st_easy) [];
    init_state_test
      "both players have 0 energy at the beginning of the game in easy \
       mode"
      (energy 1 st_easy) 0;
    init_state_test
      "both players have 0 energy at the beginning of the game in easy \
       mode"
      (energy 2 st_easy) 0;
    init_state_test "medium mode has max ship of 5" (max_ship st_med) 5;
    init_state_test "medium mode has max bomb of 5" (max_bomb st_med) 5;
    init_state_test
      "ships list is empty for both players initially in medium mode"
      (get_ship_1 st_med) [];
    init_state_test
      "ships list is empty for both players initially in medium mode"
      (get_ship_2 st_med) [];
    init_state_test
      "bombs list is empty for both players initially in medium mode"
      (bomb_lst_1 st_med) [];
    init_state_test
      "bombs list is empty for both players initially in medium mode"
      (bomb_lst_2 st_med) [];
    init_state_test
      "player 1 has 0 energy at the beginning of the game in medium \
       mode"
      (energy 1 st_med) 0;
    init_state_test
      "player 2 has 0 energy at the beginning of the game in medium \
       mode"
      (energy 2 st_med) 0;
    init_state_test "hard mode has max ship of 8" (max_ship st_hard) 8;
    init_state_test "hard mode has max bomb of 8" (max_bomb st_hard) 8;
    init_state_test "ships list is empty for player 1 in hard mode"
      (get_ship_1 st_hard) [];
    init_state_test "ships list is empty for player 2 in hard mode"
      (get_ship_2 st_hard) [];
    init_state_test "bomb list is empty for player 1 in hard mode"
      (bomb_lst_1 st_hard) [];
    init_state_test "bomb list is empty for player 2 in hard mode"
      (bomb_lst_2 st_hard) [];
    init_state_test
      "player 1 has 0 energy at the beginning of the game in hard mode"
      (energy 1 st_hard) 0;
    init_state_test
      "player 2 has 0 energy at the beginning of the game in hard mode"
      (energy 2 st_hard) 0;
  ]

let get_ship_type_tests =
  [
    get_ship_type_test "ship type of 'carrier' is Carrier" "carrier"
      Carrier;
    get_ship_type_test "ship type of 'battleship' is Battleship"
      "battleship" Battleship;
    get_ship_type_test "ship type of 'submarine' is Submarine"
      "submarine" Submarine;
    get_ship_type_test "ship type of 'destroyer' is Destroyer"
      "destroyer" Destroyer;
    get_ship_type_test
      "ship type of anything other than 'carrier' 'battleship' or \
       'submarine' is Destoyer"
      "abcd" Destroyer;
  ]

let ship_size_tests =
  [
    ship_size_test "size of a carrier ship is 5" Carrier 5;
    ship_size_test "size of a battleship ship is 4" Battleship 4;
    ship_size_test "size of a submarine ship is 3" Submarine 3;
    ship_size_test "size of a destroyer ship is 2" Destroyer 2;
  ]

let energy_tests =
  [
    energy_test "energy of initial state is 0 for player 1 in easy mode"
      1 (init_state "easy") 0;
    energy_test "energy of initial state is 0 for player 2 in easy mode"
      2 (init_state "easy") 0;
    energy_test
      "energy of initial state is 0 for player 1 in medium mode" 1
      (init_state "medium") 0;
    energy_test
      "energy of initial state is 0 for player 2 in medium mode" 2
      (init_state "medium") 0;
    energy_test "energy of initial state is 0 for player 1 in hard mode"
      1 (init_state "hard") 0;
    energy_test "energy of initial state is 0 for player 2 in hard mode"
      2 (init_state "hard") 0;
  ]

let change_energy_tests =
  [
    change_energy_test
      "changing the energy of initial state of player 1 in easy mode \
       by 3 changed the energy to 3"
      1 (init_state "easy") 3 3;
    change_energy_test
      "changing the energy of initial state of player 2 in easy mode \
       by 3 changed the energy to 3"
      2 (init_state "easy") 3 3;
    change_energy_test
      "changing the energy of initial state of player 1 in medium mode \
       by 3 changed the energy to 3"
      1 (init_state "medium") 3 3;
    change_energy_test
      "changing the energy of initial state of player 2 in medium mode \
       by 3 changed the energy to 3"
      2 (init_state "medium") 3 3;
    change_energy_test
      "changing the energy of initial state of player 1 in hard mode \
       by 3 changed the energy to 3"
      1 (init_state "hard") 3 3;
    change_energy_test
      "changing the energy of initial state of player 2 in hard mode \
       by 3 changed the energy to 3"
      2 (init_state "hard") 3 3;
    change_energy_test
      "changing the energy of initial state of player 1 in easy mode \
       by -3 changed the energy to -3"
      1 (init_state "easy") (-3) (-3);
    change_energy_test
      "changing the energy of initial state of player 2 in easy mode \
       by -3 changed the energy to -3"
      2 (init_state "easy") (-3) (-3);
    change_energy_test
      "changing the energy of initial state of player 1 in medium mode \
       by -3 changed the energy to -3"
      1 (init_state "medium") (-3) (-3);
    change_energy_test
      "changing the energy of initial state of player 2 in medium mode \
       by -3 changed the energy to -3"
      2 (init_state "medium") (-3) (-3);
    change_energy_test
      "changing the energy of initial state of player 1 in hard mode \
       by -3 changed the energy to -3"
      1 (init_state "hard") (-3) (-3);
    change_energy_test
      "changing the energy of initial state of player 2 in hard mode \
       by -3 changed the energy to -3"
      2 (init_state "hard") (-3) (-3);
    change_energy_test
      "changing the energy of initial state of player 1 in easy mode \
       by 0 did not change the energy"
      1 (init_state "easy") 0 0;
    change_energy_test
      "changing the energy of initial state of player 2 in easy mode \
       by 0 did not change the energy"
      2 (init_state "easy") 0 0;
    change_energy_test
      "changing the energy of initial state of player 1 in medui mode \
       by 0 did not change the energy"
      1 (init_state "medium") 0 0;
    change_energy_test
      "changing the energy of initial state of player 2 in medium mode \
       by 0 did not change the energy"
      2 (init_state "medium") 0 0;
    change_energy_test
      "changing the energy of initial state of player 1 in hard mode \
       by 0 did not change the energy"
      1 (init_state "hard") 0 0;
    change_energy_test
      "changing the energy of initial state of player 2 in hard mode \
       by 0 did not change the energy"
      2 (init_state "hard") 0 0;
  ]

let add_energy_tests =
  [
    add_energy_test
      "adding energy of 3 to player 1 in initial state of game mode \
       easy is 3"
      1 (init_state "easy") 3 3;
    add_energy_test
      "adding energy of 3 to player 2 in initial state of game mode \
       easy is 3"
      2 (init_state "easy") 3 3;
    add_energy_test
      "adding energy of 3 to player 1 in initial state of game mode \
       medium is 3"
      1 (init_state "medium") 3 3;
    add_energy_test
      "adding energy of 3 to player 2 in initial state of game mode \
       medium is 3"
      2 (init_state "medium") 3 3;
    add_energy_test
      "adding energy of 3 to player 1 in initial state of game mode \
       hard is 3"
      1 (init_state "hard") 3 3;
    add_energy_test
      "adding energy of 3 to player 2 in initial state of game mode \
       hard is 3"
      2 (init_state "hard") 3 3;
    add_energy_test
      "adding energy of -1 to player 1 in initial state of game mode \
       easy is 3"
      1 (init_state "easy") (-1) (-1);
    add_energy_test
      "adding energy of -1 to player 2 in initial state of game mode \
       easy is 3"
      2 (init_state "easy") (-1) (-1);
    add_energy_test
      "adding energy of -1 to player 1 in initial state of game mode \
       medium is 3"
      1 (init_state "medium") (-1) (-1);
    add_energy_test
      "adding energy of -1 to player 2 in initial state of game mode \
       medium is 3"
      2 (init_state "medium") (-1) (-1);
    add_energy_test
      "adding energy of -1 to player 1 in initial state of game mode \
       hard is 3"
      1 (init_state "hard") (-1) (-1);
    add_energy_test
      "adding energy of -1 to player 2 in initial state of game mode \
       hard is 3"
      2 (init_state "hard") (-1) (-1);
    add_energy_test
      "adding 0 energy to player 1 in initial state of game mode easy \
       does not change the energy"
      1 (init_state "easy") 0 0;
    add_energy_test
      "adding 0 energy to player 2 in initial state of game mode easy \
       does not change the energy"
      2 (init_state "easy") 0 0;
    add_energy_test
      "adding 0 energy to player 1 in initial state of game mode \
       medium does not change the energy"
      1 (init_state "medium") 0 0;
    add_energy_test
      "adding 0 energy to player 2 in initial state of game mode \
       medium does not change the energy"
      2 (init_state "medium") 0 0;
    add_energy_test
      "adding 0 energy to player 1 in initial state of game mode hard \
       does not change the energy"
      1 (init_state "hard") 0 0;
    add_energy_test
      "adding 0 energy to player 2 in initial state of game mode hard \
       does not change the energy"
      2 (init_state "hard") 0 0;
  ]

let add_ship_rep_tests =
  [
    add_ship_rep_test
      "adding ship of size 5 to start position 1 1 with orientation \
       'h' makes a ship [ (5, 1); (4, 1); (3, 1); (2, 1); (1, 1) ]"
      [] (1, 1) 5 "h" 0
      [ (1, 5); (1, 4); (1, 3); (1, 2); (1, 1) ];
    add_ship_rep_test
      "adding ship of size 5 to start position 1 1 with orientation \
       'v' makes a ship [ (5, 1); (4, 1); (3, 1); (2, 1); (1, 1) ]"
      [] (1, 1) 5 "v" 0
      [ (5, 1); (4, 1); (3, 1); (2, 1); (1, 1) ];
    add_ship_rep_test
      "adding ship of size 4 to start position 1 1 with orientation \
       'h' makes a ship [ (1, 4); (1, 3); (1, 2); (1, 1) ]"
      [] (1, 1) 4 "h" 0
      [ (1, 4); (1, 3); (1, 2); (1, 1) ];
    add_ship_rep_test
      "adding ship of size 4 to start position 1 1 with orientation \
       'v' makes a ship [ (4, 1); (3, 1); (2, 1); (1, 1) ]"
      [] (1, 1) 4 "v" 0
      [ (4, 1); (3, 1); (2, 1); (1, 1) ];
    add_ship_rep_test
      "adding ship of size 3 to start position 1 1 with orientation \
       'h' makes a ship [ (1, 3); (1, 2); (1, 1) ]"
      [] (1, 1) 3 "h" 0
      [ (1, 3); (1, 2); (1, 1) ];
    add_ship_rep_test
      "adding ship of size 3 to start position 1 1 with orientation \
       'v' makes a ship [ (3, 1); (2, 1); (1, 1) ]"
      [] (1, 1) 3 "v" 0
      [ (3, 1); (2, 1); (1, 1) ];
    add_ship_rep_test
      "adding ship of size 2 to start position 1 1 with orientation \
       'h' makes a ship [ (1, 2); (1, 1) ]"
      [] (1, 1) 2 "h" 0
      [ (1, 2); (1, 1) ];
    add_ship_rep_test
      "adding ship of size 2 to start position 1 1 with orientation \
       'v' makes a ship [ (2, 1); (1, 1) ]"
      [] (1, 1) 2 "v" 0
      [ (2, 1); (1, 1) ];
    add_ship_rep_test
      "adding ship of size 5 to start position 3 3 with orientation \
       'v' makes a ship [(3,7);(3,6);(3,5);(3,4);(3,3)]"
      [] (3, 3) 5 "h" 0
      [ (3, 7); (3, 6); (3, 5); (3, 4); (3, 3) ];
    add_ship_rep_test
      "adding ship of size 5 to start position 3 3 with orientation \
       'h' makes a ship [(7,3);(6,3);(5,3);(4,3);(3,3)]"
      [] (3, 3) 5 "v" 0
      [ (7, 3); (6, 3); (5, 3); (4, 3); (3, 3) ];
    add_ship_rep_test
      "adding ship of size 4 to start position 3 3 with orientation \
       'v' makes a ship [(3,6);(3,5);(3,4);(3,3)]"
      [] (3, 3) 4 "h" 0
      [ (3, 6); (3, 5); (3, 4); (3, 3) ];
    add_ship_rep_test
      "adding ship of size 4 to start position 3 3 with orientation \
       'h' makes a ship [(6,3);(5,3);(4,3);(3,3)]"
      [] (3, 3) 4 "v" 0
      [ (6, 3); (5, 3); (4, 3); (3, 3) ];
    add_ship_rep_test
      "adding ship of size 3 to start position 3 3 with orientation \
       'v' makes a ship [(3,5);(3,4);(3,3)]"
      [] (3, 3) 3 "h" 0
      [ (3, 5); (3, 4); (3, 3) ];
    add_ship_rep_test
      "adding ship of size 3 to start position 3 3 with orientation \
       'h' makes a ship [(6,3);(5,3);(4,3);(3,3)]"
      [] (3, 3) 3 "v" 0
      [ (5, 3); (4, 3); (3, 3) ];
    add_ship_rep_test
      "adding ship of size 2 to start position 3 3 with orientation \
       'v' makes a ship [(3,5);(3,4);(3,3)]"
      [] (3, 3) 2 "h" 0
      [ (3, 4); (3, 3) ];
    add_ship_rep_test
      "adding ship of size 2 to start position 3 3 with orientation \
       'h' makes a ship [(6,3);(5,3);(4,3);(3,3)]"
      [] (3, 3) 2 "v" 0
      [ (4, 3); (3, 3) ];
  ]

(** [make_ship_test name orientation ship_type start_p expected_output]
    constructs an OUnit test named [name] that asserts the quality of
    [expected_output] with [make_ship orientation ship_type start_p]. *)
let make_ship_test
    (name : string)
    (orientation : string)
    (ship_type : string)
    (start_p : int * int)
    (expected_output : (int * int) list) : test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists expected_output
    (make_ship orientation ship_type start_p)

(** [check_win_test name st player_num expected_output] constructs an
    OUnit test named [name] that asserts the quality of
    [expected_output] with [check_win st player_num]. *)
let check_win_test
    (name : string)
    (st : t)
    (player_num : int)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (check_win st player_num)

let game_st1 =
  make_state "easy" 3 3
    [ (1, 1); (2, 2); (3, 3) ]
    [ (1, 1); (2, 2); (3, 3) ]
    [ (1, 1); (2, 2); (3, 3); (5, 5) ]
    [] 5 5

let game_st2 =
  make_state "easy" 3 3
    [ (1, 1); (2, 2); (3, 3) ]
    [ (1, 1); (2, 2); (3, 3) ]
    [ (1, 1); (2, 2) ]
    [ (1, 1); (2, 2); (3, 3); (4, 4) ]
    5 5

let make_ship_and_check_win_tests =
  [
    make_ship_test
      "[make_ship h carrier (1,1)] yields [ (1, 1); (1, 2); (1, 3); \
       (1, 4); (1, 5)  ]"
      "h" "carrier" (1, 1)
      [ (1, 1); (1, 2); (1, 3); (1, 4); (1, 5) ];
    make_ship_test
      "[make_ship h carrier (1,1)] yields [ (1, 1); (2, 1); (3, 1); \
       (4, 1); (5, 1) ]"
      "v" "carrier" (1, 1)
      [ (1, 1); (2, 1); (3, 1); (4, 1); (5, 1) ];
    make_ship_test
      "[make_ship h battleship (1,1)] yields [ (1, 1); (1, 2); (1, 3); \
       (1, 4)) ]"
      "h" "battleship" (1, 1)
      [ (1, 1); (1, 2); (1, 3); (1, 4) ];
    make_ship_test
      "[make_ship h battleship (1,1)] yields [ (1, 1); (2, 1); (3, 1); \
       (4, 1) ]"
      "v" "battleship" (1, 1)
      [ (1, 1); (2, 1); (3, 1); (4, 1) ];
    make_ship_test
      "[make_ship h submarine (1,1)] yields [ (1, 1); (1, 2); (1, 3) ]"
      "h" "submarine" (1, 1)
      [ (1, 1); (1, 2); (1, 3) ];
    make_ship_test
      "[make_ship h submarine (1,1)] yields [ (1, 1); (2, 1); (3, 1) ]"
      "v" "submarine" (1, 1)
      [ (1, 1); (2, 1); (3, 1) ];
    make_ship_test
      "[make_ship h destroyer (1,1)] yields [(1, 2); (1, 1) ]" "h"
      "destroyer" (1, 1)
      [ (1, 1); (1, 2) ];
    make_ship_test
      "[make_ship h destroyer (1,1)] yields [ (2, 1); (1, 1) ]" "v"
      "destroyer" (1, 1)
      [ (1, 1); (2, 1) ];
    check_win_test "[check_win game_st1 1] yields true" game_st1 1 true;
    check_win_test "[check_win game_st1 2] yields false" game_st1 2
      false;
    check_win_test "[check_win game_st1 1] yields false" game_st2 1
      false;
    check_win_test "[check_win game_st1 2] yields true" game_st2 2 true;
  ]

let add_ship_in_state_tests =
  [
    add_ship_in_state_test
      "adding carrier to an initial state in easy mode to 1 1 to \
       player 1's ships will produce a state in which player 1 has a \
       ship occupying the first row of the grid"
      (init_state "easy") (1, 1) 1 "carrier" "h"
      (make_state "easy" 3 3
         [ (1, 5); (1, 4); (1, 3); (1, 2); (1, 1) ]
         [] [] [] 0 0);
    add_ship_in_state_test
      "adding carrier to an initial state in easy mode to 1 1 to \
       player 2's ships will produce a state in which player 2 has a \
       ship occupying the first row of the grid"
      (init_state "easy") (1, 1) 2 "carrier" "h"
      (make_state "easy" 3 3 []
         [ (1, 5); (1, 4); (1, 3); (1, 2); (1, 1) ]
         [] [] 0 0);
    add_ship_in_state_test
      "adding battleship to an initial state in easy mode to 1 1 to \
       player 1's ships will produce a state in which player 1 has a \
       ship occupying the first row of the grid"
      (init_state "easy") (1, 1) 1 "battleship" "h"
      (make_state "easy" 3 3
         [ (1, 4); (1, 3); (1, 2); (1, 1) ]
         [] [] [] 0 0);
    add_ship_in_state_test
      "adding battleship to an initial state in easy mode to 1 1 to \
       player 1's ships will produce a state in which player 2 has a \
       ship occupying the first row of the grid"
      (init_state "easy") (1, 1) 2 "battleship" "h"
      (make_state "easy" 3 3 []
         [ (1, 4); (1, 3); (1, 2); (1, 1) ]
         [] [] 0 0);
    add_ship_in_state_test
      "adding submarine to an initial state in easy mode to 1 1 to \
       player 1's ships will produce a state in which player 1 has a \
       ship occupying the first row of the grid"
      (init_state "easy") (1, 1) 1 "submarine" "h"
      (make_state "easy" 3 3 [ (1, 3); (1, 2); (1, 1) ] [] [] [] 0 0);
    add_ship_in_state_test
      "adding submarine to an initial state in easy mode to 1 1 to \
       player 2's ships will produce a state in which player 2 has a \
       ship occupying the first row of the grid"
      (init_state "easy") (1, 1) 2 "submarine" "h"
      (make_state "easy" 3 3 [] [ (1, 3); (1, 2); (1, 1) ] [] [] 0 0);
    add_ship_in_state_test
      "adding destroyer to an initial state in easy mode to 1 1 to \
       player 1's ships will produce a state in which player 1 has a \
       ship occupying the first row of the grid"
      (init_state "easy") (1, 1) 1 "destroyer" "h"
      (make_state "easy" 3 3 [ (1, 2); (1, 1) ] [] [] [] 0 0);
    add_ship_in_state_test
      "adding destroyer to an initial state in easy mode to 1 1 to \
       player 2's ships will produce a state in which player 2 has a \
       ship occupying the first row of the grid"
      (init_state "easy") (1, 1) 2 "destroyer" "h"
      (make_state "easy" 3 3 [] [ (1, 2); (1, 1) ] [] [] 0 0);
    add_ship_in_state_test
      "adding destroyer to an initial state in easy mode to 1 1 to \
       player 2's ships will produce a state in which player 2 has a \
       ship occupying the first column of the grid"
      (init_state "easy") (1, 1) 2 "destroyer" "v"
      (make_state "easy" 3 3 [] [ (2, 1); (1, 1) ] [] [] 0 0);
  ]

let add_bomb_in_state_tests =
  [
    add_bomb_in_state_test
      "adding a single coordinate bomb to player 1's bomb list to an \
       initial state in easy mode will produce a state in which player \
       1 has one element in their bomb list and energy of -1"
      (init_state "easy")
      [ (2, 2) ]
      1 1
      (make_state "easy" 3 3 [] [] [ (2, 2) ] [] (-1) 0);
    add_bomb_in_state_test
      "adding a single coordinate bomb to player 2's bomb list to an \
       initial state in easy mode will produce a state in which player \
       2 has one element in their bomb list and energy of -1"
      (init_state "easy")
      [ (2, 2) ]
      2 1
      (make_state "easy" 3 3 [] [] [] [ (2, 2) ] 0 (-1));
    add_bomb_in_state_test
      "adding a column bomb in column 1 to player 1's bomb list to an \
       initial state in easy mode will produce a state in which player \
       1 has elements (5,1), (4,1), (3,1), (2,1), (1, 1) in their bomb \
       list and energy of -4"
      (init_state "easy")
      [ (1, 1); (2, 1); (3, 1); (4, 1); (5, 1) ]
      1 4
      (make_state "easy" 3 3 [] []
         [ (1, 1); (2, 1); (3, 1); (4, 1); (5, 1) ]
         [] (-4) 0);
    add_bomb_in_state_test
      "adding a column bomb in column 1 to player 2's bomb list to an \
       initial state in easy mode will produce a state in which player \
       2 has elements (5,1), (4,1), (3,1), (2,1), (1, 1) in their bomb \
       list and energy of -4"
      (init_state "easy")
      [ (1, 1); (2, 1); (3, 1); (4, 1); (5, 1) ]
      2 4
      (make_state "easy" 3 3 [] [] []
         [ (1, 1); (2, 1); (3, 1); (4, 1); (5, 1) ]
         0 (-4));
    add_bomb_in_state_test
      "adding a cross bomb with center (3, 3) to player 1's bomb list \
       to an initial state in easy mode will produce a state in which \
       player 1 has elements (3, 1), (3, 2), (3, 4), (3, 5), (1, 3), \
       (2, 3), (4, 3), (5, 3) in their bomb list and energy of -1"
      (init_state "easy")
      [ (3, 1); (3, 2); (3, 4); (3, 5); (1, 3); (2, 3); (4, 3); (5, 3) ]
      1 6
      (make_state "easy" 3 3 [] []
         [
           (1, 3); (2, 3); (3, 1); (3, 2); (3, 4); (3, 5); (4, 3); (5, 3);
         ]
         [] (-6) 0);
    add_bomb_in_state_test
      "adding a cross bomb with center (3, 3) to player 2's bomb list \
       to an initial state in easy mode will produce a state in which \
       player 2 has elements (3, 1), (3, 2), (3, 4), (3, 5), (1, 3), \
       (2, 3), (4, 3), (5, 3) in their bomb list and energy of -1"
      (init_state "easy")
      [ (3, 1); (3, 2); (3, 4); (3, 5); (1, 3); (2, 3); (4, 3); (5, 3) ]
      2 6
      (make_state "easy" 3 3 [] [] []
         [
           (1, 3); (2, 3); (3, 1); (3, 2); (3, 4); (3, 5); (4, 3); (5, 3);
         ]
         0 (-6));
    add_bomb_in_state_test
      "adding a single row bomb in row 1 to player 1's bomb list to an \
       initial state in easy mode will produce a state in which player \
       1 has elements (1, 1), (1, 2), (1, 3), (1, 4), (1, 5) in their \
       bomb list and energy of -4"
      (init_state "easy")
      [ (1, 1); (1, 2); (1, 3); (1, 4); (1, 5) ]
      1 4
      (make_state "easy" 3 3 [] []
         [ (1, 1); (1, 2); (1, 3); (1, 4); (1, 5) ]
         [] (-4) 0);
    add_bomb_in_state_test
      "adding a diagonal bomb to player 1's bomb list to an initial \
       state in easy mode will produce a state in which player 1 has \
       elements (1, 1), (2, 2), (3, 3), (4, 4), (5, 5), (1, 5), (2, \
       4), (4, 2), (5, 1) in their bomb list and energy of -1"
      (init_state "easy")
      [
        (1, 1);
        (2, 2);
        (3, 3);
        (4, 4);
        (5, 5);
        (1, 5);
        (2, 4);
        (4, 2);
        (5, 1);
      ]
      1 6
      (make_state "easy" 3 3 [] []
         [
           (1, 1);
           (1, 5);
           (2, 2);
           (2, 4);
           (3, 3);
           (4, 2);
           (4, 4);
           (5, 1);
           (5, 5);
         ]
         [] (-6) 0);
    add_bomb_in_state_test
      "adding a single square bomb to (3, 3) in player 1's bomb list \
       to an initial state in easy mode will produce a state in which \
       player 1 has elements (3, 2), (3, 3), (3, 4), (2, 2), (2, 3), \
       (2, 4), (4, 2), (4, 3), (4, 4) in their bomb list and energy of \
       -1"
      (init_state "easy")
      [
        (3, 2);
        (3, 3);
        (3, 4);
        (2, 2);
        (2, 3);
        (2, 4);
        (4, 2);
        (4, 3);
        (4, 4);
      ]
      1 5
      (make_state "easy" 3 3 [] []
         [
           (2, 2);
           (2, 3);
           (2, 4);
           (3, 2);
           (3, 3);
           (3, 4);
           (4, 2);
           (4, 3);
           (4, 4);
         ]
         [] (-5) 0);
  ]

let make_grid_tests =
  [
    make_grid_test
      "grid representation of initialized easy game is \
       [[Grid.make_location; Grid.make_location; Grid.make_location; \
       Grid.make_location; Grid.make_location]; [Grid.make_location; \
       Grid.make_location; Grid.make_location; Grid.make_location; \
       Grid.make_location]; [Grid.make_location; Grid.make_location; \
       Grid.make_location; Grid.make_location; Grid.make_location]; \
       [Grid.make_location; Grid.make_location; Grid.make_location; \
       Grid.make_location; Grid.make_location]; [Grid.make_location; \
       Grid.make_location; Grid.make_location; Grid.make_location; \
       Grid.make_location]"
      5 5
      [|
        [|
          Grid.make_location;
          Grid.make_location;
          Grid.make_location;
          Grid.make_location;
          Grid.make_location;
        |];
        [|
          Grid.make_location;
          Grid.make_location;
          Grid.make_location;
          Grid.make_location;
          Grid.make_location;
        |];
        [|
          Grid.make_location;
          Grid.make_location;
          Grid.make_location;
          Grid.make_location;
          Grid.make_location;
        |];
        [|
          Grid.make_location;
          Grid.make_location;
          Grid.make_location;
          Grid.make_location;
          Grid.make_location;
        |];
        [|
          Grid.make_location;
          Grid.make_location;
          Grid.make_location;
          Grid.make_location;
          Grid.make_location;
        |];
      |];
  ]

let ship_in_bounds_tests =
  [
    ship_in_bounds_test
      "carrier ship placed vertically on (5, 5) on easy mode is out of \
       bounds"
      5 5 (make_grid 5 5) "carrier" "v" false;
    ship_in_bounds_test
      "carrier ship placed vertically on (1, 1) on easy mode is not \
       out of bounds"
      1 1 (make_grid 5 5) "carrier" "v" true;
    ship_in_bounds_test
      "carrier ship placed horizontally on (5, 5) on easy mode is out of \
       bounds"
      5 5 (make_grid 5 5) "carrier" "h" false;
    ship_in_bounds_test
      "carrier ship placed horizontally on (1, 1) on easy mode is not \
       out of bounds"
      1 1 (make_grid 5 5) "carrier" "h" true;
    ship_in_bounds_test
      "carrier ship placed with invalid orientation on (5, 5) on easy mode is 
      out of bounds"
      5 5 (make_grid 5 5) "carrier" "x" false;
  ]

let collision_detect_tests =
  [
    collision_detect_test
      "passing in a list of ship coordinates for player 1 that do not \
       have any coordinates in common with existing ships in player \
       1's ship will make collision_detect false"
      [
        (1, 1);
        (1, 2);
        (1, 3);
        (1, 4);
        (1, 5);
        (2, 1);
        (2, 2);
        (2, 3);
        (2, 4);
        (2, 5);
      ]
      (make_state "easy" 3 3
         [
           (3, 1);
           (3, 2);
           (3, 3);
           (3, 4);
           (3, 5);
           (4, 1);
           (4, 2);
           (4, 3);
           (4, 4);
           (4, 5);
           (5, 1);
           (5, 2);
           (5, 3);
           (5, 4);
           (5, 5);
         ]
         [] [] [] 0 0)
      1 false;
    collision_detect_test
      "passing in a list of ship coordinates for player 1 that have \
       coordinates in common with existing ships in player 1's ship \
       will make collision_detect true"
      [ (5, 5) ]
      (make_state "easy" 3 3
         [
           (3, 1);
           (3, 2);
           (3, 3);
           (3, 4);
           (3, 5);
           (4, 1);
           (4, 2);
           (4, 3);
           (4, 4);
           (4, 5);
           (5, 1);
           (5, 2);
           (5, 3);
           (5, 4);
           (5, 5);
         ]
         [] [] [] 0 0)
      1 true;
  ]

let battleship_game_tests =
  List.flatten
    [
      init_state_tests;
      get_ship_type_tests;
      ship_size_tests;
      energy_tests;
      change_energy_tests;
      add_energy_tests;
      add_ship_rep_tests;
      add_ship_in_state_tests;
      add_bomb_in_state_tests;
      make_grid_tests;
      ship_in_bounds_tests;
      collision_detect_tests;
      make_ship_and_check_win_tests;
    ]

let suite =
  "test suite for A2"
  >::: List.flatten [ command_tests; battleship_game_tests ]

let _ = run_test_tt_main suite
