type ptype = TNormal | TFire | TWater
type peff = ENormal | ENotVery | ESuper

let mult_of_eff = function ESuper -> 2.0 | ENormal -> 1. | ENotVery -> 0.5

let eff = function
  | TFire, TFire | TWater, TWater | TFire, TWater -> ENotVery
  | TWater, TFire -> ESuper
  | _ -> ENormal

type mon = { name : string; hp : float; ptype : ptype }

let charmander = { name = "Charmander"; hp = 50.; ptype = TFire }
let squirtle = { name = "Squirtle"; hp = 50.; ptype = TWater }
let attack_eff p1 p2 = eff (p1.ptype, p2.ptype) |> mult_of_eff

let rec battle p1 p2 =
  match (p1.hp, p2.hp) with
  | _, 0. -> (p1, p2)
  | 0., _ -> (p2, p1)
  | _, _ ->
      battle
        { p1 with hp = p1.hp -. attack_eff p2 p1 }
        { p2 with hp = p2.hp -. attack_eff p1 p2 }

let _ = battle charmander squirtle
