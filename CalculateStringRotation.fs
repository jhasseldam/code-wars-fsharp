module CalculateStringRotation

let private rotate (str : string) =
  str.Substring(str.Length - 1) + str.Substring(0, str.Length - 1)

let ShiftedDiff (first : string) (second : string) =
  let max = first.Length
  let rec countRotationsUntilMatch source rotations =
    if rotations > max then -1
    else
      match source = second with
      | true -> rotations
      | false -> countRotationsUntilMatch (rotate source) (rotations + 1)
  countRotationsUntilMatch first 0