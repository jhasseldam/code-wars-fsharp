// https://www.codewars.com/kata/tribonacci-sequence
// (6kyu)
module Tribonacci

let tribonacci' signature n =
    let rec calculateSeq x y z counter lst =
        if counter = n
        then List.rev lst
        else
            let next = x + y + z
            calculateSeq y z next (counter + 1) (next :: lst)
    match (signature, n) with
    | _, 0 -> []
    | x :: _, 1 -> [x]
    | x :: y :: _, 2 -> [x; y]
    | x :: y :: [z], _ -> calculateSeq x y z 3 (signature |> List.rev)
    | _ -> failwith "invalid input"