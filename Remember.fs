module Remember

type Position = int
type Occurrence =
  | Once
  | Several of Position

let private registerChar charMap pos (c : char) =
  match Map.tryFind c charMap with
  | Some v when v = Once ->
      Map.remove c charMap
      |> Map.add c (Several pos), pos + 1
  | None -> Map.add c Once charMap, pos
  | _ -> charMap, pos

let _rem (str:string) =
  str.ToCharArray()
  |> Array.toList
  |> List.fold (fun (m, p) c -> registerChar m p c) (Map.empty, 0)
  |> fun (m, _) -> Map.toList m
  |> List.filter (fun (_, v) -> v <> Once)
  |> List.sortBy (fun (_, v) -> v)
  |> List.map (fun (k, _) -> k)