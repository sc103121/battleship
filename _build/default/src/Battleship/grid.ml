type hit =
  | Bombed
  | Safe

type occupied =
  | Ship
  | NoShip

type location = {
  ship_stat : occupied;
  bomb_stat : hit;
}

type grid = location array array

let make_location = { ship_stat = NoShip; bomb_stat = Safe }

let has_ship l =
  match l with
  | { ship_stat = ship; bomb_stat = bombed } ->
      if ship = Ship then true else false

let was_bombed l =
  match l with
  | { ship_stat = ship; bomb_stat = bombed } ->
      if bombed = Bombed then true else false

let set_ship l =
  match l with
  | { ship_stat = no_ship; bomb_stat = bs } ->
      { ship_stat = Ship; bomb_stat = bs }

let set_bomb l =
  match l with
  | { ship_stat = s; bomb_stat = no_bomb } ->
      { ship_stat = s; bomb_stat = Bombed }
