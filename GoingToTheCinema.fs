// https://www.codewars.com/kata/going-to-the-cinema
// 7 (kyu)
module GoingToTheCinema
open System

let movie (card: int) (ticket: int) (perc: float): int =
  let initialTicketPrice = float ticket * perc
  let initialTotalPrice = float card + initialTicketPrice
  let cheaperThanSystemA (p :float) count = Math.Ceiling(p) |> int < (ticket * count) 
  let rec calculateVisits totalPrice prevTicketPrice count =
    match cheaperThanSystemA totalPrice count with
    | true -> count
    | false ->
      let nextTicketPrice = prevTicketPrice * perc
      calculateVisits (totalPrice + nextTicketPrice) nextTicketPrice (count + 1)
  calculateVisits initialTotalPrice initialTicketPrice 1
  