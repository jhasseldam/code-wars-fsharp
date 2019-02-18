// https://www.codewars.com/kata/which-are-in
// (6kyu)
module WhichAreIn

let private anyContains strList (criteria : string) =
  List.exists (fun (w : string) -> w.Contains(criteria)) strList

let inArray (a1: string list) (a2: string list) =
  a1
  |> List.filter (anyContains a2)
  |> List.distinct
  |> List.sort