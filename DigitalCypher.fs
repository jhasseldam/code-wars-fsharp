// https://www.codewars.com/kata/digital-cypher
// (7kyu)
module DigitalCypher
open System

let private cypherMap =
  [ ('a', 1); ('b', 2); ('c', 3); ('d', 4); ('e', 5)
    ('f', 6); ('g', 7); ('h', 8); ('i', 9); ('j', 10)
    ('k', 11); ('l', 12); ('m', 13); ('n', 14); ('o', 15)
    ('p', 16); ('q', 17); ('r', 18); ('s', 19); ('t', 20)
    ('u', 21); ('v', 22); ('w', 23); ('x', 24); ('y', 25); ('z', 26)
  ] |> Map.ofList

let private encodeChar c =
  Map.find c cypherMap

let private toKeyList (n : int) =
  (string n).ToCharArray()
  |> Array.toList
  |> List.map (string >> Int32.Parse)

let private applyKey keyList encodedList =
  let rec applyKey' keys inputInts (outputInts : int list) =
    match (keys, inputInts) with
    | [k], e::es -> applyKey' keyList es ((k + e)::outputInts)
    | k::ks, e::es -> applyKey' ks es ((k + e)::outputInts)
    | _, _ -> outputInts |> List.rev
  applyKey' keyList encodedList []

let encode (str : string) n =
  str.ToCharArray()
  |> Array.toList
  |> List.map encodeChar
  |> applyKey (toKeyList n)