// https://www.codewars.com/kata/first-non-repeating-character
// (6kyu)
module FirstNonRepeatingCharacter

let firstNonRepeatingLetter str =
  str
  |> Seq.countBy (fun c -> (string c).ToLower())
  |> Seq.where (fun (_, count) -> count = 1)
  |> Seq.map (fun (s, _) -> s)
  |> Seq.tryHead
  |> Option.defaultValue ""