#load "../common.fs"
open Common

open System.Collections.Generic

let grid = 
  readLines "nums.txt" 
  |> Seq.map (fun (s: string) -> s.ToCharArray() |> Array.map (fun i -> int i - int '0')) 
  |> Seq.toArray

let big_add (i: int[]) (j: int[]): (int[]) =
  let ensure_same_size (i: int[]) (j: int[]) = 
    if Array.length i <> Array.length j then
      let diff = Array.length i - Array.length j
      if diff > 0 then
        (i, Array.concat [[|for _ in 1 .. diff -> 0|]; j])
      else
        (Array.concat [[|for _ in 1 .. (-1*diff) -> 0|]; i], j)
    else
      (i,j)

  let (ii, jj) = ensure_same_size i j
  let (ri, rj) = (Array.rev ii, Array.rev jj)
  let digits = new List<int>()
  let mutable carry = 0

  for idx in 0 .. ri.Length-1 do
    let sum = ri[idx] + rj[idx] + carry
    digits.Add(sum % 10)
    carry <- sum / 10

  while carry <> 0 do
    digits.Add(carry % 10)
    carry <- carry / 10

  Array.rev (digits |> Seq.toArray)

let op1 = [|0|]
let op2 = [|9;9|]
printfn "Adding 9 and 99 %A" (big_add op1 op2)

grid 
|> Array.reduce (fun acc elem -> big_add acc elem) 
|> Array.take 10 
|> printfn "Result %A"