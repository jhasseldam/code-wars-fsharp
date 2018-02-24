module Tribonacci

// Tribonacci Sequence (6kyu)

let tribonacci' signature n =
    let rec calculateSeq x y z counter lst =
        if counter = n
        then List.rev lst
        else
            let next = x + y + z 
            calculateSeq y z next (counter + 1) (next :: lst)
    match signature with
    | x :: y :: [z] -> calculateSeq x y z 3 (signature |> List.rev)
    | _ -> failwith "invalid input"