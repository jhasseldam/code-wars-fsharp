// https://www.codewars.com/kata/help-the-bookseller
// 6 (kyu)
module HelpTheBookSeller
open System

let extractCategoryAndAmount (str : string) =
  match str.Split([|' '|]) |> Array.toList with
  | category::quantity::_ -> Seq.head category |> string, Int32.Parse(quantity)
  | _ -> failwith "invalid input"

let addOrUpdateCategory map book =
  let category, quantity = extractCategoryAndAmount book
  match Map.tryFind category map with
  | Some newQuantity -> quantity + newQuantity
  | None -> quantity
  |> fun quantity' -> Map.add category quantity' map

let buildBookMap bookArray =
  Seq.fold addOrUpdateCategory Map.empty bookArray

let quantityResult map category =
  Map.tryFind category map
  |> Option.defaultValue 0
  |> sprintf "(%s : %i)" category

let stockSummary(lstOfArt: string[]) (lstOf1stLetter: string[]): string =
   if (Array.isEmpty lstOfArt)
   then String.Empty
   else
     let bookMap = buildBookMap lstOfArt
     Seq.map (quantityResult bookMap) lstOf1stLetter
     |> String.concat " - "