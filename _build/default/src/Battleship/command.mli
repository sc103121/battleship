(** Command enables parsing of player commands. *)

type row_or_col = int
(** The type [row_or_col] is an integer that represents a row or column
    number.*)

type object_phrase = int * int
(** The type [object_phrase] represents the object phrase that can be
    part of a player command. It is a pair of ints (row, column) that
    specifies a location given the row and column. Thus, no element of
    the list should contain any leading, internal, or trailing spaces.
    The list is in the same order as the words in the original player
    command. For example:

    - If the player command is ["1 2"], then the object phrase is
      [(1, 2)].

    - If the player command is ["     3 2"], then the object phrase is
      again [(3, 2)].

    An [object_phrase] is not permitted to be the empty pair. *)

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
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
(** Raised when a command not in the specified format is parsed. *)

val parse : string -> command
(** [parse str] parses a player's input into a [command], as follows.
    The first word (i.e., consecutive sequence of non-space characters)
    of [str] becomes the verb. The word must be all lower case. If the
    verb is followed by two integers, then the two integers form an
    object phrase of integer tuples, specifying a coordinate on the game
    board. The two integers might also be followed by two strings, the
    first string represents the type of the ship being placed and must
    be ["carrier", "destroyer", "submarine" or "battleship"]. The last
    string is a single character, and it must be either ["v"] if the
    ship's orientation is vertical, and ["h"] if the ship's orientation
    is horizontal. If the first word is followed by one integer, the
    word is of type row_or_col, which specifies a row or column.
    Examples:

    - [parse "carrier 1 2 h"] is [Ship (1,2), "carrier", "h")]
    - [parse "battleship 1 2 h"] is [Ship (1,2), "battleship", "h")]
    - [parse "submarine 1 2 h"] is [Ship (1,2), "submarine", "h")]
    - [parse "destroyer 1 2 h"] is [Ship (1,2), "destroyer", "h")]
    - [parse "bomb 1 2"] is [Bomb (1,2)]
    - [parse "easy"] is [Easy].
    - [parse "quit"] is [Quit].

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines,
    etc.), and must be all lowercase.

    Raises: [Invalid_Format] if the command is malformed. A command is
    {i invalid_format} if the verb is neither "place ship", "bomb", "end
    turn" "quit" or if the verb is "quit" or "end turn" and there is a
    non-empty object phrase, or if the verb is "place ship" or "bomb"
    and there is an empty object phrase or an object phrase that is not
    of type int.*)
