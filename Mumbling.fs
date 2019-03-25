// https://www.codewars.com/kata/mumbling
// 7 (kyu)
module Mumbling

open System

let private repeatLetter strAndCounter (letter : char) = 
  let str, counter = strAndCounter
  String.replicate counter (string letter)
  |> fun w -> w.ToLower()
  |> fun w -> str + w.Substring(0, 1).ToUpper() + w.Substring(1) + "-"
  |> fun w -> w, counter + 1

let accum (s: string): string =
  s.ToCharArray()
  |> Seq.fold repeatLetter (String.Empty, 1) 
  |> fun (str, _) -> str.TrimEnd('-')