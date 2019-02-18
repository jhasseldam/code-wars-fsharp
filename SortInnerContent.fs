// https://www.codewars.com/kata/srot-the-inner-ctonnet-in-dsnnieedcg-oredr
// (6kyu)
module SortInnerContent
open System

let private sortingNeeded (chars : char []) = chars.Length > 3

let private sortWord (word : string) =
  let characters = word.ToCharArray()
  match sortingNeeded characters with
  | false -> word
  | true ->
      let firstLetter = Array.head characters
      let lastLetter = Array.last characters
      Array.sub characters 1 (characters.Length - 2)
      |> Array.sortDescending
      |> String
      |> fun sortedMiddle -> sprintf "%c%s%c" firstLetter sortedMiddle lastLetter

let sortTheInnerContent (words : string) : string =
  words.Split(' ')
  |> Array.map sortWord
  |> fun sortedWords -> String.Join(" ", sortedWords)