[@@@ocamlformat "disable = true"]

type object_phrase = int * int
type row_or_col = int

type command =
  | Place_Ship of object_phrase * string * string
  | Bomb of object_phrase
  | Bomb_col of row_or_col
  | Bomb_row of row_or_col
  | Bomb_cross of object_phrase
  | Bomb_square of object_phrase
  | Bomb_diagonal
  | Easy
  | Medium
  | Hard
  | End_Turn
  | Energy
  | Quit

exception Invalid_Format

(* [invalid_format lst] returns false if [lst] is not a two-element
   list, or its elements are not both ints. [invalid_format lst] returns
   true otherwise.*)
let invalid_format lst =
  if List.length lst = 2 then
    match lst with
    | [ a; b ] -> begin
        match (int_of_string a, int_of_string b) with
        | exception s -> true
        | _ -> false
      end
    | _ -> true
  else true

(* [invalid_format_ship lst] returns false if the lst stores the
   starting coordinate of the ship in its first and second element, and
   the orientation of the ship as either 'v' or 'o' as its third
   element. [invalid_format_ship lst] returns true otherwise. *)
let invalid_format_ship lst =
  if List.length lst = 3 then
    match lst with
    | [ a; b; c ] -> begin
        match (int_of_string a, int_of_string b) with
        | exception s -> true
        | _ -> if c = "h" || c = "v" then false else true
      end
    | _ -> true
  else true

(* [invalid_bomb_col_or_row lst] returns false if [lst] stores the row
   or column value in string; [invalid_format lst] returns true
   otherwise.*)
let invalid_bomb_col_or_row lst =
  if List.length lst = 1 then
    match lst with
    | [ a ] -> begin
        match int_of_string a with
        | exception s -> true
        | _ -> false
      end
    | _ -> true
  else true

(** [parse_valid lst] parses a string list [lst] that represents a valid
    command and turns it into an actual command of type [Command]. The string 
    list [lst] must have the following form: The first element of [lst] 
    represents a valid verb ["bomb", "bomb_row", "bomb_col", "bomb_cross", 
    "bomb_diagonal", "bomb_square", "end_turn", "energy", "quit"], a correct 
    game state of ["easy", "medium", "hard"] or a correct ship type ["carrier", 
    "destroyer", "submarine" or "battleship"]. The rest of the elements of [lst] 
    must be of type object_phrase, row_or_col, or 
    object_phrase * string * string, as specified above in the type command. *)
let parse_valid_cmd lst =
  let t = List.tl lst in
  let pair_coordinate =
    if List.tl t <> [] then
      ( t |> List.hd |> int_of_string,
        t |> List.tl |> List.hd |> int_of_string )
    else (t |> List.hd |> int_of_string, 0) in
  let coordinate = t |> List.hd |> int_of_string in
  let o =
    if List.length t >= 3 then t |> List.tl |> List.tl |> List.hd
    else "f" in
  match List.hd lst with
  | "carrier" | "battleship" | "submarine" | "destroyer" ->
      Place_Ship (pair_coordinate, List.hd lst, o)
  | "bomb" -> Bomb pair_coordinate
  | "bomb_col" -> Bomb_col coordinate
  | "bomb_cross" -> Bomb_cross pair_coordinate
  | "bomb_square" -> Bomb_square pair_coordinate
  | _ -> Bomb_row coordinate

(* [place_or_bomb lst] is a corresponding placing ship command or
   bombing command based on [lst]; Raises [Invalid_Format] if lst is in
   wrong format.*)
let place_or_bomb lst =
  match lst with
  | [] -> raise Invalid_Format
  | h :: t -> (
      if
        invalid_format t && h <> "bomb_col" && h <> "bomb_row"
        && h <> "carrier" && h <> "battleship" && h <> "submarine"
        && h <> "destroyer"
      then raise Invalid_Format
      else if
        (h = "bomb_col" || h = "bomb_row") && invalid_bomb_col_or_row t
      then raise Invalid_Format
      else if
        (h = "carrier" || h = "battleship" || h = "submarine"
       || h = "destroyer")
        && invalid_format_ship t
      then raise Invalid_Format
      else
        parse_valid_cmd lst)

(* [quit_or_end_turn lst] pattern matches against the element of lst and
   returns the corresponding commands. Raises Invalid_Format if the lst
   is not an one-element lst.*)
let quit_or_end_turn lst =
  match lst with
  | [] -> raise Invalid_Format
  | h :: t -> (
      if t <> [] then raise Invalid_Format
      else
        match h with
        | "quit" -> Quit
        | "energy" -> Energy
        | "end" -> End_Turn
        | "easy" -> Easy
        | "medium" -> Medium
        | "hard" -> Hard
        | _ -> Bomb_diagonal)

let parse str =
  let splitted_list =
    String.split_on_char ' ' str |> List.filter (fun x -> x <> "")
  in
  match splitted_list with
  | [] -> raise Invalid_Format
  | h :: t ->
      if
        h = "quit" || h = "end" || h = "easy" || h = "medium"
        || h = "hard" || h = "bomb_diagonal" || h = "energy"
      then quit_or_end_turn splitted_list
      else if
        h = "carrier" || h = "battleship" || h = "submarine"
        || h = "destroyer" || h = "bomb" || h = "bomb_col"
        || h = "bomb_row" || h = "bomb_cross" || h = "bomb_square"
      then place_or_bomb splitted_list
      else raise Invalid_Format
