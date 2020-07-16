module GreedIsGood
// https://www.codewars.com/kata/5270d0d18625160ada0000e4/train/fsharp
// 5 kyu

let scoreThreeOfTheSame = function
  | 1 -> 1000
  | 6 -> 600
  | 5 -> 500
  | 4 -> 400
  | 3 -> 300
  | 2 -> 200
  | _ -> 0

let scoreSideCount sideCount =
  let rec loop score sideCount =
    match sideCount with
    | x, count when count >= 3 -> loop (scoreThreeOfTheSame x) (x, count - 3)
    | 1, count -> score + count * 100
    | 5, count -> score + count * 50
    | _ -> score + 0
  loop 0 sideCount

let score (dice : int list) =
  List.countBy id dice
  |> List.fold (fun score sideCount -> score + (scoreSideCount sideCount)) 0 

type ValidEmail = Email of string

let sendEmail (email : ValidEmail) =
 ()

let a = sendEmail ""


