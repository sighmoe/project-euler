#load "../common.fs"
#load "poker.fsx"

open Common
open Poker

let hands = readLines "poker.txt" 
let summaries: seq<Summary> = Seq.map eval hands
let get_p1_wins (summaries: seq<Summary>): int =
  let read_summary = function
  | (_, _, P1Win) -> 1
  | _ -> 0
  in Seq.fold (fun (acc: int) (elem: Summary) -> acc + (read_summary elem)) 0 summaries
printfn "P1 wins: %d" (summaries |> get_p1_wins) 


