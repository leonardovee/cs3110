type primary_color = Red | Green | Blue

let r = Red

type point = float * float

type shape =
  | Circle of { center : point; radius : float }
  | Rectangle of { lower_left : point; upper_right : point }
  | Point of point

let c1 = Circle { center = (0., 0.); radius = 1. }
let r1 = Rectangle { lower_left = (-1., -1.); upper_right = (1., 1.) }
let p1 = (31., 10.)
let avg a b = (a +. b) /. 2.

let center s =
  match s with
  | Circle { center; _ } -> center
  | Rectangle { lower_left = x_ll, y_ll; upper_right = x_ur, y_ur } ->
      (avg x_ll x_ur, avg y_ll y_ur)
  | Point point -> point
