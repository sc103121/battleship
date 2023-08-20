(** battleship_game.ml tracks the state of the game and contains
    functions for editing the state of the game. *)

type g_mode
(** The abstract type representing the game mode. *)

type t
(** The abstract type representing the state of the game. *)

(** The type [ship_type] represents the type of a ship. *)
type ship_type =
  | Carrier
  | Battleship
  | Submarine
  | Destroyer

val init_state : string -> t
(** [init_state mode] is the initial state of the game with difficulty
    [mode]. *)

val max_ship : t -> int
(** [max_ship state] is the maximum number of ships each player can
    place dependant on the difficulty mode in game state [state]. *)

val max_bomb : t -> int
(** [max_bomb state] is the maximum number of bombs each player can
    place in a turn dependant on the difficulty mode in game state
    [state]. *)

val make_grid : int -> int -> Grid.location array array
(** [make_grid rows cols] will initialize a location array array with
    [rows] arrays of length [cols] in which each location will have a
    ship status of NoShip and a bomb status of Safe.
    [make_grid rows cols] is this resulting initialized location array
    array. *)

val print_grid : Grid.location array array -> string -> unit
(** [print_grid player_or_opp] will print the string representation of a
    location array array. The string [player_or_opp] will determine
    whether or not the ship status should be printed. When
    [player_or_opp] is 'player' the ships will be displayed and
    represnted by string 'S' and in all other cases, locations with
    ships will be represented by string '~'. *)

val add_ship : int -> int -> Grid.location array array -> unit
(** [add_ship row col] will change the status of the ship at grid
    location with index ([row] - 1, [col] - 1) to Ship. *)

val add_bomb : int -> int -> Grid.location array array -> unit
(** [add_bomb row col] will change the status of the ship at grid
    location with index ([row] - 1, [col] - 1) to Bombed. *)

val add_ship_vertical :
  int -> int -> Grid.location array array -> string -> int -> unit
(** [add_ship_vertical row col grid t_str num_added] alters the board
    [grid] to add a representation of a ship of ship type [t_str] which
    starts at the position row, col and occupies the increasing spaces
    of the col [col] in [grid]. The number of coordinates representing
    the positions the ship occupies is represented by [num_added] which
    is the accumulator starting at 0. *)

val add_ship_horizontal :
  int -> int -> Grid.location array array -> string -> int -> unit
(** [add_ship horizontal row col grid t_str num_added] alters the board
    [grid] to add a representation of a ship of ship type [t_str] which
    starts at the position row, col and occupies the increasing spaces
    of the row [row] in [grid]. The number of coordinates representing
    the positions the ship occupies is represented by [num_added] which
    is the accumulator starting at 0. *)

val ship_in_bounds :
  int -> int -> Grid.location array array -> string -> string -> bool
(** [ship_in_bounds row col grid t_str orientation] is true if the ship
    being placed with starting location ([row], [col]), orienation
    [orientation], and ship type [t_str] does not extend beyond the
    dimensions of the grid [grid] and false otherwise. *)

val check_win : t -> int -> bool
(** [check win st player_num] is true if player [player_num] has sunk
    all of the opposite player's ships in state [st]. *)

val add_ship_in_state : t -> int * int -> int -> string -> string -> t
(** [add_ship_in_state st start_pos player ship_type orientation] is the
    state resulting from adding the ship represented by an int * int
    list, with starting position [start_pos] and orientation
    [orientation] to the list of player [player]'s ships in game state
    [st]. *)

val add_bomb_in_state : t -> (int * int) list -> int -> int -> t
(** [add_bomb_in_state st added_bomb player remove_energy_amt] is the
    state resulting from adding the bomb [added_bomb] which is
    represented by an int * int to player [player]'s list of bombs in
    game state [st] and subtracting the energy [remove_energy_amt] from
    [st]'s current energy amount. *)

val bomb_lst_1 : t -> (int * int) list
(** [bomb_lst_1 state] is a list of player 1's list of bombs,
    represented by int * ints in game state [state]. *)

val bomb_lst_2 : t -> (int * int) list
(** [bomb_lst_2 state] is a list of player 2's list of bombs,
    represented by int * ints in game state [state]. *)

val collision_detect : (int * int) list -> t -> int -> bool
(** [collision_detect ship game_st player_num] is true if player
    [player_num] is attempting to place a ship [ship] on his own board
    of ships in game state [game_st] where a ship already exists. *)

val energy : int -> t -> int
(** [energy player_num st] is the number of energy points player
    [player_num] currently has in the current state [st]. *)

val change_energy : int -> t -> int -> unit
(** [change_energy player_num st changed_amt] adds [changed_amt] of
    energy points to player [player_num]'s current energy in the current
    state [st]. *)

val make_ship : string -> string -> int * int -> (int * int) list
(** [make_ship orientation ship_type start_p] is a ship represented by
    an int * int list with the first element as the starting position
    [start_p], the length determined by the ship's type [ship_type], and
    the subsequent elements determined by the orienatation of the ship
    [orientation]. *)

val get_mode : t -> g_mode
(** [get_mode st] is the game mode of state [st]. *)

val get_ship_1 : t -> (int * int) list
(** [get_ship_1 st] is the list of player 1's ships in state [st]. *)

val get_ship_2 : t -> (int * int) list
(** [get_ship_2 st] is the list of player 2's ships in state [st]. *)

val get_ship_type : string -> ship_type
(** [get_ship_type t_str] is the size of ship with ship type [ship_str]. *)

val ship_size : ship_type -> int
(** [ship_size s_type] is the number of grid spaces taken up by a ship
    of type [s_type]. *)

val add_energy : int -> int -> t -> int
(** [add_energy amt p st] is the state with [amt] energy added to player
    [p]'s total energy in state [st]. *)

val add_ship_rep :
  (int * int) list ->
  int * int ->
  int ->
  string ->
  int ->
  (int * int) list
(** [add_ship_rep ship start_pos size o loc_added_acc] is the list of
    grid spaces taken up by a ship [ship] of size [size] and orientation
    [o] starting at grid space [start_pos]. *)

val make_state :
  string ->
  int ->
  int ->
  (int * int) list ->
  (int * int) list ->
  (int * int) list ->
  (int * int) list ->
  int ->
  int ->
  t
(** [make_state g_mode max_ship max_bomb ship_1 ship_2 bomb_1 bomb_2
    energy_1 energy_2]
    is the state with mode [g_mode], maximum number of ships to place
    per turn [max_ship], maximum number of bombs to place per turn
    [max_bomb], player 1's ships [ship_1], player 2's ships [ship_2],
    player 1's bombs [bomb_1], player 2's bombs [bomb_2]. *)
