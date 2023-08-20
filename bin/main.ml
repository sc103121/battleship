[@@@ocamlformat "disable = true"]
(** main module uses functions in command module and battleship_game module.
main module run the game program. *)

open Battleship
open Battleship.Command
open Battleship.Battleship_game

exception OutOfRange

(** [display_board_given_mode m] will take in [m] as the command for
    mode. [Easy] will make a [grid], display this [grid], and return a
    tuple of [grid], [x], [y], and [t], where [grid] is of type grid,
    [x] is the size in rows, [y] is the size in columns, and [t] is the
    type of the game state. Raise exception if [m] is not a mode command*)
let display_board_given_mode m =
  match m with
  | Easy ->
      let board = make_grid 5 5 in
      print_grid board "player";
      (board, 5, 5, init_state "easy")
  | Medium ->
      let board = make_grid 7 7 in
      print_grid board "player";
      (board, 7, 7, init_state "medium")
  | Hard ->
      let board = make_grid 10 10 in
      print_grid board "player";
      (board, 10, 10, init_state "hard")
  | _ -> failwith "Not a type."

(** [display_board_in_game size] will take in [size] as the user's
    string command input and match the input to an appropriate command.
    User input 'quit' will quit the game. User input 'easy', will make a
    [grid], display this [grid], and return a tuple of [grid], [x], [y],
    and [t], where [grid] is of type grid, [x] is the size in rows, [y]
    is the size in columns, and [t] is the type of the game state. Any
    invalid command will cause the game program to reprompt the user for
    a valid command.*)
let rec display_board_in_game size =
  match parse size with
  | Easy
  | Medium
  | Hard ->
      display_board_given_mode (parse size)
  | Quit ->
      print_endline "Bye.\n";
      exit 0
  | exception s -> (
      print_endline
        "Wrong format. Try again with an input of the correct format\n";
      print_string "> ";
      match read_line () with
      | size -> display_board_in_game size)
  | _ -> (
      print_endline
        "Wrong command. Try again with an input of the correct command\n";
      print_string "> ";
      match read_line () with
      | size -> display_board_in_game size)

(** [add_bomb_on_board board rows cols location] will add a bomb to
    grid's location at [location], which is a tuple that store (x
    coordinate, y coordinate) and then display this updated grid that
    represents information about [board]. The grid is of dimension
    [rows] by [cols]. *)
let rec add_bomb_on_board board rows cols location =
  match location with
  | row, col when row > rows || col > cols ->
      raise (Failure "Out of range")
  | row, col -> add_bomb row col board

(** [print_bomb_directions loop_num] will print out the instruction on how to
    enter a bomb command in the correct format. *)
let print_bomb_directions loop_num =
  print_endline
    ("Please enter the location you want to place your "
   ^ string_of_int loop_num
   ^ " bomb. For example, to place a bomb in the box on row '1'  and \
      column '2',  you should type bomb 1 2\n");
  print_string "> ";
  print_endline
    "Type quit to quit the game. Type end to end the turn.\n"

(** [bomb_energy command] will return the energy cost for the
    corresponding bombing command. *)
let bomb_energy command =
  match command with
  | Bomb _ -> 1
  | Bomb_col _
  | Bomb_row _ ->
      4
  | Bomb_square _ -> 5
  | Bomb_cross _
  | Bomb_diagonal ->
      6
  | _ -> -1

(** [set_bomb game_st board rows cols loop_num max_bomb player_num] will
    ask for the user's string command input to set bomb on the
    opponent's grid. User input 'quit' will quit the game. User input
    'end' will move to the next game stage. User input 'bomb x y', where
    x and y are valid integer input for a location that the user want to
    put a bomb, will place a bomb on that location on the opponent's
    grid and display this grid. Any invalid command will cause the game
    program to reprompt the user for a valid command. [game_st] is the
    current state of the game. [board] is the opponent's grid, which is
    of size [rows] by [cols]. [loop_num] is the current turn number of
    the player. [max_bomb] is the maximum number of bombs the player can
    place in a single turn. [player_num] is 1 if it's player 1's turn,
    or 2 if it's player 2's turn. *)
let rec set_bomb game_st board rows cols loop_num max_bomb player_num =
  if check_win game_st player_num then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      ("PLAYER " ^ string_of_int player_num ^ " WINS! \n");
    exit 0)
  else if loop_num > max_bomb || energy player_num game_st <= 0 then
    game_st
  else
    try
      proceed_to_bomb game_st board rows cols loop_num max_bomb
        player_num
    with
    | _ ->
        print_endline "Invalid Format. Please try again.\n";
        set_bomb game_st board rows cols loop_num max_bomb player_num

(** [proceed_to_bomb game_st board rows cols loop_num max_bomb player_num]
    will ask for the user's string command input to set bomb on the
    opponent's grid. User input 'quit' will quit the game. User input
    'end' will move to the next game stage. User input 'bomb x y', where
    x and y are valid integer input for a location that the user want to
    put a bomb, will place a bomb on that location on the opponent's
    grid and display this grid. Any invalid command will cause the game
    program to reprompt the user for a valid command. [game_st] is the
    current state of the game. [board] is the opponent's grid, which is
    of size [rows] by [cols]. [loop_num] is the current turn number of
    the player. [max_bomb] is the maximum number of bombs the player can
    place in a single turn. [player_num] is 1 if it's player 1's turn,
    or 2 if it's player 2's turn. *)
and proceed_to_bomb game_st board rows cols loop_num max_bomb player_num =
  print_bomb_directions loop_num;
  let bomb_lst = if player_num = 1 then bomb_lst_1 else bomb_lst_2 in
  let command = parse (read_line ()) in
  let bomb_energy = bomb_energy command in
  if
    energy player_num game_st < bomb_energy
    && energy player_num game_st > 0
  then begin
    print_endline "You do not have enough energy. Try again.\n";
    set_bomb game_st board rows cols loop_num max_bomb player_num
  end
  else
    detect_command command game_st bomb_lst command player_num
      bomb_energy board rows cols loop_num max_bomb

(** [detect_command command game_st bomb_lst command player_num
    bomb_energy board rows cols loop_num max_bomb] will print out corresponding
    messages based on the command to keep the game going. *)
and detect_command command game_st bomb_lst command player_num bomb_energy board 
rows cols loop_num max_bomb =
  match command with
  | Bomb _
  | Bomb_cross _
  | Bomb_square _
  | Bomb_col _
  | Bomb_row _
  | Bomb_diagonal ->
      bomb_command game_st bomb_lst command player_num bomb_energy board
        rows cols loop_num max_bomb
  | _ ->
      other_command command game_st bomb_lst command player_num
        bomb_energy board rows cols loop_num max_bomb

(** [other_command command game_st bomb_lst command player_num
    bomb_energy board rows cols loop_num max_bomb] will print out corresponding
    messages based on the commands non related to bombing that are in the 
    bombing phase in order to keep the game going. *)
and other_command command game_st bomb_lst command player_num bomb_energy board 
rows cols loop_num max_bomb =
  match command with
  | Quit ->
      print_endline "Bye.\n";
      exit 0
  | End_Turn ->
      print_endline "Placing bombs turn ends.\n";
      game_st
  | Energy ->
      print_string
        ("Your current energy: "
        ^ string_of_int (energy player_num game_st)
        ^ "\n");
      set_bomb game_st board rows cols loop_num max_bomb player_num
  | _ ->
      print_endline "Wrong command. Please try again.\n";
      set_bomb game_st board rows cols loop_num max_bomb player_num

(** [bomb_command command game_st bomb_lst command player_num
    bomb_energy board rows cols loop_num max_bomb] will print out corresponding
    messages based on the bomb related command to keep the game going. *)
and bomb_command game_st bomb_lst command player_num bomb_energy board rows
cols loop_num max_bomb =
  match command with
  | Bomb (row, col) ->
      bomb game_st bomb_lst row col player_num bomb_energy board rows
        cols loop_num max_bomb
  | Bomb_cross (row, col) ->
      bomb_cross game_st bomb_lst row col player_num bomb_energy board
        rows cols loop_num max_bomb
  | Bomb_square (row, col) ->
      bomb_square game_st row col board rows cols player_num bomb_energy
        loop_num max_bomb
  | Bomb_col col ->
      bomb_col game_st bomb_lst col player_num bomb_energy board rows
        cols loop_num max_bomb
  | Bomb_row row ->
      bomb_row game_st bomb_lst row player_num bomb_energy board rows
        cols loop_num max_bomb
  | Bomb_diagonal ->
      bomb_diagonal game_st board rows cols player_num bomb_energy
        loop_num max_bomb
  | _ -> failwith "Invalid bomb command."

(** [bomb game_st bomb_lst row col player_num bomb_energy board rows cols
    loop_num max_bomb] will reprompt the player to enter valid bomb command
    if the current command is not valid; the bomb will be placed on the board
    and the updated board will be printed out with the amount of energy used
    if the bomb command is valid. *)
and bomb game_st bomb_lst row col player_num bomb_energy board rows cols 
    loop_num max_bomb =
  if List.mem (row, col) (game_st |> bomb_lst) then begin
    print_endline "This space already has a bomb. Try again.\n";
    set_bomb game_st board rows cols loop_num max_bomb player_num
  end
  else begin
    add_bomb_on_board board rows cols (row, col);
    print_grid board "opponent";
    print_endline "You used 1 energy point";
    let new_game_st =
      add_bomb_in_state game_st [ (row, col) ] player_num bomb_energy
    in
    set_bomb new_game_st board rows cols (loop_num + 1) max_bomb
      player_num
  end

(** [bomb_cross game_st bomb_lst row col player_num bomb_energy board rows cols
    loop_num max_bomb] will reprompt the player to enter valid bomb_cross
    command if the current command is not valid; the bomb_cross will be placed
    on the board and the updated board will be printed out with the amount of
    energy used if the bomb_cross command is valid. *)
and bomb_cross game_st bomb_lst row col player_num bomb_energy board rows cols 
    loop_num max_bomb =
  let new_bomb_lst_ref = ref [] in
  for i = 1 to rows do
    add_bomb_on_board board rows cols (i, col);
    new_bomb_lst_ref := (i, col) :: !new_bomb_lst_ref
  done;
  for j = 1 to cols do
    add_bomb_on_board board rows cols (row, j);
    new_bomb_lst_ref := (row, j) :: !new_bomb_lst_ref
  done;
  print_grid board "opponent";
  print_endline "You used 6 energy point";
  let new_game_st =
    add_bomb_in_state game_st !new_bomb_lst_ref player_num bomb_energy
  in
  set_bomb new_game_st board rows cols (loop_num + 1) max_bomb
    player_num

(** [bomb_square game_st bomb_lst row col player_num bomb_energy board rows cols
    loop_num max_bomb] will reprompt the player to enter valid bomb_square
    command if the current command is not valid; the bomb_square will be placed
    on the board and the updated board will be printed out with the amount of
    energy used if the bomb_square command is valid. *)
and bomb_square game_st row col board rows cols player_num bomb_energy loop_num 
    max_bomb =
  let new_bomb_lst_ref = ref [] in
  let low_i = if row - 1 < 1 then 1 else row - 1 in
  let high_i = if row + 1 > rows then rows else row + 1 in
  let low_j = if col - 1 < 1 then 1 else col - 1 in
  let high_j = if col + 1 > cols then cols else col + 1 in
  for i = low_i to high_i do
    for j = low_j to high_j do
      add_bomb_on_board board rows cols (i, j);
      new_bomb_lst_ref := (i, j) :: !new_bomb_lst_ref
    done
  done;
  print_grid board "opponent";
  print_endline "You used 5 energy point";
  let new_game_st =
    add_bomb_in_state game_st !new_bomb_lst_ref player_num bomb_energy
  in
  set_bomb new_game_st board rows cols (loop_num + 1) max_bomb
    player_num

(** [bomb_col game_st bomb_lst col player_num bomb_energy board rows cols
    loop_num max_bomb] will reprompt the player to enter valid bomb_col
    command if the current command is not valid; the bomb_col will be placed
    on the board and the updated board will be printed out with the amount of
    energy used if the bomb_col command is valid. *)
and bomb_col game_st bomb_lst col player_num bomb_energy board rows cols 
    loop_num max_bomb =
  let new_bomb_lst_ref = ref [] in
  for i = 1 to rows do
    add_bomb_on_board board rows cols (i, col);
    new_bomb_lst_ref := (i, col) :: !new_bomb_lst_ref
  done;
  print_grid board "opponent";
  print_endline "You used 4 energy point";
  let new_game_st =
    add_bomb_in_state game_st !new_bomb_lst_ref player_num bomb_energy
  in
  set_bomb new_game_st board rows cols (loop_num + 1) max_bomb
    player_num

(** [bomb_row game_st bomb_lst row player_num bomb_energy board rows cols
    loop_num max_bomb] will reprompt the player to enter valid bomb_row
    command if the current command is not valid; the bomb_row will be placed
    on the board and the updated board will be printed out with the amount of
    energy used if the bomb_row command is valid. *)
and bomb_row game_st bomb_lst row player_num bomb_energy board rows cols 
    loop_num max_bomb =
  let new_bomb_lst_ref = ref [] in
  for i = 1 to cols do
    add_bomb_on_board board rows cols (row, i);
    new_bomb_lst_ref := (row, i) :: !new_bomb_lst_ref
  done;
  print_grid board "opponent";
  print_endline "You used 4 energy point";
  let new_game_st =
    add_bomb_in_state game_st !new_bomb_lst_ref player_num bomb_energy
  in
  set_bomb new_game_st board rows cols (loop_num + 1) max_bomb
    player_num

(** [bomb_diagonal game_st board rows cols player_num bomb_energy loop_num
    max_bomb] will reprompt the player to enter valid bomb_diagonal
    command if the current command is not valid; the bomb_diagonal will be
    placed on the board and the updated board will be printed out with the
    amount of energy used if the bomb_diagonal command is valid. *)
and bomb_diagonal game_st board rows cols player_num bomb_energy loop_num 
    max_bomb =
  let new_bomb_lst_ref = ref [] in
  for i = 1 to rows do
    add_bomb_on_board board rows cols (i, i);
    add_bomb_on_board board rows cols (i, rows + 1 - i);
    new_bomb_lst_ref := (i, i) :: (i, rows + 1 - i) :: !new_bomb_lst_ref
  done;
  print_grid board "opponent";
  print_endline "You used 6 energy point";
  let new_game_st =
    add_bomb_in_state game_st !new_bomb_lst_ref player_num bomb_energy
  in
  set_bomb new_game_st board rows cols (loop_num + 1) max_bomb
    player_num

(** [display_ship_on_board board rows cols location ship_type orientation]
    will add a ship to grid's location at [location] and based on
    [ship_type] and [orientation], which is a tuple that store (x
    coordinate, y coordinate) of the head of the ship and then display
    this updated grid that represents information about [board]. The
    grid is [rows] by [cols]. *)
let rec display_ship_on_board board rows cols location ship_type orientation =
  match location with
  | row, col when row > rows || col > cols ->
      raise (Failure "Out of range")
  | row, col
    when ship_in_bounds row col board ship_type orientation = false ->
      raise (Failure "Out of range")
  | row, col ->
      if orientation = "v" then
        add_ship_vertical row col board ship_type 0
      else add_ship_horizontal row col board ship_type 0;
      print_grid board "player"

(** [set_ship game_st board rows cols loop_num taken max_ship
    player_num] will ask for the user's string command input to set ship
    on this user's own grid. User input 'quit' will quit the game. User
    input 'end' will move to the next game stage. User input '<ship
    type> x y', where x and y are valid integer input for a location
    that the user want to put a ship, will place a ship of the
    corresponding ship type on that location on the user's grid and
    display this grid. Any invalid command will cause the game program
    to reprompt the user for a valid command. game_st] is the current
    state of the game. [board] is the opponent's grid, which is of size
    [rows] by [cols]. [loop_num] is the current turn number of the
    player. [taken] stores all starting coordinates of ships placed by
    this player. [max_ship] is the maximum number of ships the player
    can place in a single turn. [player_num] is 1 if it's player 1's
    turn, or 2 if it's player 2's turn.*)
let rec set_ship game_st board rows cols loop_num taken max_ship player_num =
  try
    ship_instruction loop_num;
    match parse (read_line ()) with
    | Quit | End_Turn | Energy as c ->
      cmd_not_ship game_st board rows cols loop_num taken max_ship player_num c
    | Place_Ship ((row, col), h, o) when List.mem (row, col) taken ->
        print_endline "This space already has a ship. Try again.\n";
        set_ship game_st board rows cols loop_num taken max_ship
          player_num
    | Place_Ship ((row, col), ship_type, orientation) ->
        place_valid_ship game_st board rows cols loop_num taken max_ship
          player_num row col ship_type orientation
    | _ ->
        print_endline "Wrong command. Please try again.\n";
        set_ship game_st board rows cols loop_num taken max_ship
          player_num
  with
  | _ ->
      print_endline "Invalid Format. Please try again.\n";
      set_ship game_st board rows cols loop_num taken max_ship player_num

(** [cmd_not_ship game_st board rows cols loop_num taken max_ship player_num
    cmd] will print out corresponding messages based on the commands non
    related to placing ship that are in the placing ship phase
    in order to keep the game going. *)
and cmd_not_ship game_st board rows cols loop_num taken
max_ship player_num cmd =
  match cmd with
  | Quit ->
      print_endline "Bye.\n";
      exit 0
  | End_Turn ->
      print_endline "Placing ships turn ends.\n";
      game_st
  | _ ->
      print_string
        ("Your current energy: "
        ^ string_of_int (energy player_num game_st)
        ^ "\n");
      set_ship game_st board rows cols loop_num taken max_ship
        player_num

(** [ship_instruction loop_num] will print out corresponding instruction for
    how to type down a valid placing ship command. *)
and ship_instruction loop_num =
  print_endline
    ("Please enter the location you want to place your ship #"
   ^ string_of_int loop_num
   ^ ". There are four types of ship you can place: carrier (occupies \
      5 grids), battleship (occupies 4 grids), submarine (occupies 3 \
      grids), destroyer (occupies 2 grids). There are two \
      orientations: horizontal, which is represented by letter h, and \
      vertical, which is represented by letter v. For example, to \
      place a carrier on the board on row '1' and column '2' \
      horizontally, you should type 'carrier 1 2 h.'\n");
  print_endline
    "Type quit to quit the game. Type end to end the turn.\n";
  print_string "> "

(** [place_valid_ship game_st board rows cols loop_num taken max_ship
    player_num row col ship_type orientation] will print out corresponding
    messages based on the commands related to placing ship that are in the
    placing ship phase in order to keep the game going. *)
and place_valid_ship game_st board rows cols loop_num taken max_ship player_num 
row col ship_type orientation =
  if loop_num = max_ship then game_st
  else
    let new_ship = make_ship orientation ship_type (row, col) in
    if collision_detect new_ship game_st player_num then begin
      print_endline "This space already has a ship. Try again.\n";
      set_ship game_st board rows cols loop_num taken max_ship
        player_num
    end
    else
      let updated_game_st =
        add_ship_in_state game_st (row, col) player_num ship_type
          orientation
      in
      display_ship_on_board board rows cols (row, col) ship_type
        orientation;
      set_ship updated_game_st board rows cols (loop_num + 1)
        ((row, col) :: taken) max_ship player_num

(** [bomb_loop board1 board2 row col max_bomb], in turns, displays the
    opponent's grid (opponent's ships will not be visible on the grid),
    asks the user to input a valid command string to set bomb on the
    opponent's grid, and then display the opponent's updated grid based
    on the user's input command string. [board1] is the opponent's grid
    for player 2; [board2] is the opponent's grid for player 1. The size
    of the board is [row] by [col]. [max_bomb] is the maximum number of
    bombs each player can place each turn. *)
let rec bomb_loop game_st board1 board2 row col max_bomb =
  print_endline "Player 1's turn to place bombs! \n";
  print_grid board2 "opponent";
  change_energy 1 game_st 5;
  let updated_game_st = set_bomb game_st board2 row col 1 max_bomb 1 in
  print_endline "Player 2's turn to place bombs! \n";
  print_grid board1 "opponent";
  change_energy 2 updated_game_st 5;
  let updated_game_st2 =
    set_bomb updated_game_st board1 row col 1 max_bomb 2
  in
  bomb_loop updated_game_st2 board1 board2 row col max_bomb

(** [start_game mode board num_of_rows num_of_cols st] launches the game by
    setting the game state [st] to be of mode [mode], with a board [board]
    with[num_of_rows] rows and [num_of_cols] columns for the two
    players to place ships and bombs on. *)
let start_game mode board num_of_rows num_of_cols st =
  let game_state = init_state mode in
  let ship_board_p1 = Array.map Array.copy board in
  let ship_board_p2 = Array.map Array.copy board in
  print_endline "Player 1's turn to place ships! \n";
  print_grid ship_board_p1 "player";
  let updated_game_st =
    set_ship game_state ship_board_p1 num_of_rows num_of_cols 1
      [] (max_ship st) 1
  in
  print_endline "Player 2's turn to place ships! \n";
  print_grid ship_board_p2 "player";
  let updated_game_st2 =
    set_ship updated_game_st ship_board_p2 num_of_rows
      num_of_cols 1 [] (max_ship st) 2
  in
  bomb_loop updated_game_st2 ship_board_p1 ship_board_p2
    num_of_rows num_of_cols (max_bomb st)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\nWelcome to the CS3110 simple Battleship game!\n";
  print_endline
    "What game mode do you want? Type easy, medium or hard.\n";
  print_string "> ";
  match read_line () with
  | mode -> (
      match display_board_in_game mode with
      | board, num_of_rows, num_of_cols, st ->
          start_game mode board num_of_rows num_of_cols st)

(* Execute the game engine. *)
let () = main ()
