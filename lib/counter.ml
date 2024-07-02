let counter = ref 0

let next () =
  counter := !counter + 1;
  !counter
