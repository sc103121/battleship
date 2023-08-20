(** grid.ml tracks the grid representation of the game, which includes
    every location on the grid and which locations have been bombed.
    Includes functions for placing ships and bombs on the grid. *)

type hit
(** Type hit reveals if the location has or has not been hit by a bomb. *)

type occupied
(** Type occupied reveals if the location contains or does not contain a
    ship. *)

type location
(** Type location is a location in the grid and has a ship status, a
    bomb status, and its row and column in the grid. *)

type grid
(** Type grid is a matrix of all locations in the game. *)

val make_location : location
(** [make_location] is an initialized location that has NoShip as its
    ship status and Safe as its bomb status. *)

val has_ship : location -> bool
(** [has_ship l] is true when location [l] has a ship and false
    otherwise. Requires that [l] is a valid location within the
    dimsensions of the grid. *)

val was_bombed : location -> bool
(** [was_bombed l] is true when location [l] was bombed and false
    otherwise. Requires that [l] is a valid location within the
    dimsensions of the grid. *)

val set_ship : location -> location
(** [set_ship l] is the location [l] with its ship status set to Ship.
    Requires that [l] is a valid location within the dimsensions of the
    grid and [l]'s current ship status is NoShip. *)

val set_bomb : location -> location
(** [set_bomb l] is the location [l] with its ship status set to Bombed.
    Requires that [l] is a valid location within the dimsensions of the
    grid and [l]'s current bomb status is Safe. *)
