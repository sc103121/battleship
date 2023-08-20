[@@@ocamlformat "disable = true"]
type g_mode =
 | Easy
 | Medium
 | Hard

type t = {
 mode : g_mode;
 max_ship : int;
 max_bomb : int;
 ship_1 : (int * int) list;
 ship_2 : (int * int) list;
 bomb_1 : (int * int) list;
 bomb_2 : (int * int) list;
 mutable energy_1 : int;
 mutable energy_2 : int;
}

type ship_type =
 | Carrier
 | Battleship
 | Submarine
 | Destroyer

let get_mode st = st.mode
let get_ship_1 st = st.ship_1
let get_ship_2 st = st.ship_2

let get_ship_type t_str =
 match t_str with
 | "carrier" -> Carrier
 | "battleship" -> Battleship
 | "submarine" -> Submarine
 | _ -> Destroyer

let ship_size ship_type =
 match ship_type with
 | Carrier -> 5
 | Battleship -> 4
 | Submarine -> 3
 | Destroyer -> 2

(** [change_energy p st amt] changes player [p]'s energy points by [amt]
   in the state [st]*)
let change_energy player_num st changed_amt =
 if player_num = 1 then st.energy_1 <- st.energy_1 + changed_amt
 else st.energy_2 <- st.energy_2 + changed_amt

(** [energy p st] is the number of energy points that player [p] has in
   the state [st]. *)
let energy player_num st =
 if player_num = 1 then st.energy_1 else st.energy_2

let init_state mode =
 let g_mode =
   if mode = "easy" then Easy
   else if mode = "medium" then Medium
   else Hard
 in
 let max =
   if mode = "easy" then 3 else if mode = "medium" then 5 else 8
 in
 {
   mode = g_mode;
   max_ship = max;
   max_bomb = max;
   ship_1 = [];
   ship_2 = [];
   bomb_1 = [];
   bomb_2 = [];
   energy_1 = 0;
   energy_2 = 0;
 }

(** [add_energy amt p st] is the number of energy points possessed by
   player [p] in state [st] after [amt] energy points are added. *)
let add_energy amount player_num st =
 if player_num = 1 then energy 1 st + amount else energy 2 st + amount

let rec add_ship_rep ship start_pos size orientation loc_added_acc =
 if loc_added_acc = size then ship
 else
   let loc_to_add =
     match start_pos with
     | r, c ->
         if orientation = "h" then (r, c + loc_added_acc)
         else (r + loc_added_acc, c)
   in
   let new_ship = loc_to_add :: ship in
   add_ship_rep new_ship start_pos size orientation (loc_added_acc + 1)

let add_ship_in_state st start_pos player ship_type orientation =
 let ship_size =
   match ship_type with
   | "carrier" -> 5
   | "battleship" -> 4
   | "submarine" -> 3
   | _ -> 2
 in let p1_ships = st.ship_1 in let p2_ships = st.ship_2 in let ship_1 =
   if player = 1 then
     add_ship_rep p1_ships start_pos ship_size orientation 0
   else st.ship_1 in let ship_2 =
   if player = 2 then add_ship_rep p2_ships start_pos ship_size orientation 0
   else st.ship_2
 in
 { mode = st.mode; max_ship = st.max_ship; max_bomb = st.max_bomb; ship_1;
 ship_2; bomb_1 = st.bomb_1; bomb_2 = st.bomb_2; energy_1 = st.energy_1;
 energy_2 = st.energy_2;
 }

let add_bomb_in_state st added_bomb player remove_energy_amt =
 let bomb_1 =
   if player = 1 then
     added_bomb @ st.bomb_1 |> List.sort_uniq Stdlib.compare else st.bomb_1
 in
 let bomb_2 =
   if player = 2 then
     added_bomb @ st.bomb_2 |> List.sort_uniq Stdlib.compare else st.bomb_2
 in
 let energy_1 =
   if player = 1 then add_energy (-remove_energy_amt) 1 st
   else st.energy_1
 in
 let energy_2 =
   if player = 2 then add_energy (-remove_energy_amt) 2 st
   else st.energy_2
 in
 { mode = st.mode; max_ship = st.max_ship; max_bomb = st.max_bomb;
   ship_1 = st.ship_1; ship_2 = st.ship_2; bomb_1; bomb_2; energy_1; energy_2;
 }

let bomb_lst_1 state = state.bomb_1
let bomb_lst_2 state = state.bomb_2
let max_ship state = state.max_ship
let max_bomb state = state.max_bomb

let make_grid rows cols =
 let l = Grid.make_location in
 let a_row = Array.make cols l in
 let a_grid = Array.make rows a_row in
 a_grid

(** [loc_to_str ships_visible l] represents a location as a string. It
   will show the location as an unbombed ship 'S' if the location has a
   ship, has not been bombed, and is being viewed by the player who
   placed the ship, 'M' if a bomb was placed but there is no ship in
   that location, 'B' is a bomb was placed and there is a ship in the
   location, and '~' in all other cases. *)
let loc_to_str ships_visible l =
 let shipstat =
   if ships_visible then
     if Grid.has_ship l && Grid.was_bombed l then "H"
     else if Grid.has_ship l && Grid.was_bombed l = false then "S"
     else if Grid.has_ship l = false && Grid.was_bombed l then "M"
     else "~"
   else if Grid.has_ship l && Grid.was_bombed l then "H"
   else if Grid.has_ship l = false && Grid.was_bombed l then "M"
   else "~"
 in
 shipstat

(** [grid_to_str g] is the string array array representation of a
   location array array [g] in which each element is represented by
   either '~', 'S', 'H', or 'M' depending on the bomb and ship status
   of each location as well as who is viewing the grid, specified by
   [ships_visible] which is true when we intend the ships to be visible
   to the player who placed them on their own board or false when we do
   not want the player to see the location of the opponent player's
   ships. *)
let grid_to_str g ships_visible =
 Array.map (Array.map (loc_to_str ships_visible)) g

(** [row_to_str row] is the string form of string array [row] with each
   element in row represented as a string seperated by a space. *)
let row_to_str row = String.concat " " (Array.to_list row) ^ "\n"

let print_grid grid player_or_opp =
 let str_grid =
   if player_or_opp = "player" then grid_to_str grid true
   else grid_to_str grid false
 in
 Array.iter
   (fun row ->
     (ANSITerminal.print_string [ ANSITerminal.blue ])
       (row |> row_to_str))
   str_grid

let add_ship row col grid =
 let location = Grid.set_ship grid.(row - 1).(col - 1) in
 grid.(row - 1).(col - 1) <- location

let add_bomb row col grid =
 let location = Grid.set_bomb grid.(row - 1).(col - 1) in
 grid.(row - 1).(col - 1) <- location

let rec add_ship_vertical row col grid t_str num_added =
 let t = get_ship_type t_str in
 let location = Grid.set_ship grid.(row - 1).(col - 1) in
 grid.(row - 1).(col - 1) <- location;
 if num_added < ship_size t - 1 then
   add_ship_vertical (row + 1) col grid t_str (num_added + 1)

let rec add_ship_horizontal row col grid t_str num_added =
 let t = get_ship_type t_str in
 let location = Grid.set_ship grid.(row - 1).(col - 1) in
 grid.(row - 1).(col - 1) <- location;
 if num_added < ship_size t - 1 then
   add_ship_horizontal row (col + 1) grid t_str (num_added + 1)

let ship_in_bounds
   row
   col
   (grid : Grid.location array array)
   t_str
   orientation =
 let t = get_ship_type t_str in
 let ship_size = ship_size t in
 let grid_cols = Array.length (Array.get grid 0) in
 let grid_rows = Array.length grid in
 match orientation with
 | "v" -> if row + ship_size - 1 <= grid_rows then true else false
 | "h" -> if col + ship_size - 1 <= grid_cols then true else false
 | _ -> false

(** [total_grid_occupied st p] is the list of all grid locations that
   are occupied by player [p]'s ships. *)
let total_grid_occupied game_st player_num =
 if player_num = 1 then game_st.ship_1 else game_st.ship_2

(** [check_overlapped l1 l2] returns true if any values in l1 are also
   in l2. If no values in l1 are also in l2, returns false. *)
let rec check_overlapped l1 l2 =
 match l1 with
 | [] -> false
 | h :: t -> List.exists (fun x -> h = x) l2 || check_overlapped t l2

let collision_detect ship game_st player_num =
 let t_grid_occupied = total_grid_occupied game_st player_num in
 check_overlapped ship t_grid_occupied

let check_win st player_num =
 let bombs = if player_num = 1 then st.bomb_1 else st.bomb_2 in
 let ships = if player_num = 1 then st.ship_2 else st.ship_1 in
 let common = List.filter (fun s -> List.mem s bombs) ships in
 List.length ships = List.length common

(** [init_locations start_p end_p lst orientation] is the list of
   locations that will contain a ship that starts at location
   [start_p], ends at location [end_p] and is of orientation
   [orientation]. Requires: [start_p] and [end_p] are valid locations
   on the grid. [orientation] is either "v" or "h". *)
let rec init_locations
   (start_p : int * int)
   (end_p : int * int)
   lst
   orientation =
 if start_p = end_p then end_p :: lst
 else
   match orientation with
   | "v" -> (
       match start_p with
       | r, c -> init_locations (r + 1, c) end_p (start_p :: lst) "v")
   | _ -> (
       match start_p with
       | r, c -> init_locations (r, c + 1) end_p (start_p :: lst) "h")

let make_ship orientation ship_type start_p =
 let t =
   match ship_type with
   | "carrier" -> Carrier
   | "battleship" -> Battleship
   | "submarine" -> Submarine
   | _ -> Destroyer
 in
 let size = ship_size t in
 let end_p =
   match start_p with
   | r, c ->
       if orientation = "h" then (r, c + size - 1)
       else (r + size - 1, c)
 in
 let l = init_locations start_p end_p [] orientation in
 l

let make_state
   (g_mode : string) max_ship max_bomb ship_1 ship_2 bomb_1 bomb_2 energy_1
   energy_2 =
 let mode =
   match g_mode with
   | "easy" -> Easy
   | "medium" -> Medium
   | _ -> Hard
 in
 { mode; max_ship; max_bomb; ship_1; ship_2;
 bomb_1; bomb_2; energy_1; energy_2}
