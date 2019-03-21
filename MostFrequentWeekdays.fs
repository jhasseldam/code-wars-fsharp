// https://www.codewars.com/kata/most-frequent-weekdays
// (6 kyu)
module MostFrequentWeekdays
open System

let updateWeekDayMap map weekday =
  match Map.tryFind weekday map with
  | Some c -> Map.add weekday (c + 1) map
  | None -> Map.add weekday 1 map

let mostFrequentDays year =
  let dt = DateTime(year, 1, 1, 0, 0, 0)
  let rec loop (nextDay : DateTime) map =
    match nextDay.Year = year with
    | true -> loop (nextDay.AddDays(1.0)) (updateWeekDayMap map nextDay.DayOfWeek)
    | false -> map
  let lst = 
    loop dt Map.empty
    |> Map.toList
    |> List.sortByDescending (fun (_, v) -> v)
  let (_, highestDayCount) = List.head lst
  List.filter (fun (_, v) -> v = highestDayCount) lst
  |> List.map (fun (k, _) -> k)
  |> List.sortBy (fun dow -> ((int dow) + 6) % 7)
  |> List.map string
