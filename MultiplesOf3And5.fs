module MutiplesOf3And5

let solve n =
  List.fold (fun acc n -> if n % 3 = 0 || n % 5 = 0 then acc+n else acc) 0 [1..n-1]