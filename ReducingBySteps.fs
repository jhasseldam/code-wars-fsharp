module ReducingBySteps

let som x y = x + y
let mini (x : int) (y : int) = if x < y then x else y
let maxi (x : int) (y : int) = if x > y then x else y
let rec gcdi x y = if y = 0 then abs x else gcdi y (x % y)
let lcmu x y = (x * y / (gcdi x y)) |> abs

let operArray fct arr init =
  let rec reduce prev lst output =
    match lst with
    | x::xs ->
        let r = fct prev x
        reduce r xs (r::output)
    | [] -> output |> List.rev
  reduce init arr []