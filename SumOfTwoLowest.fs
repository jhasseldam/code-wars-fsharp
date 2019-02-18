// https://www.codewars.com/kata/sum-of-two-lowest-positive-integers
// (7kyu)
module SumOfTwoLowest
open System

// O(n) solution

type LowestNumbers =
  {
    Lowest : int64
    SecondLowest : int64
  }

let private replaceIfNeeded numbers n =
  match (n < numbers.Lowest, n < numbers.SecondLowest) with
  | true, _ -> { Lowest = n; SecondLowest = numbers.Lowest }
  | false, true -> { numbers with SecondLowest = n }
  | _ -> numbers

let sumTwoSmallestNumbers numbers =
  let max = Int64.MaxValue
  Array.fold replaceIfNeeded { Lowest = max; SecondLowest = max } numbers
  |> fun nums -> nums.Lowest + nums.SecondLowest