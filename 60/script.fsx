#load "../common.fs"
open Common
open System.Collections.Generic

let concat_nums nums = 
  let concat (x: int) (y: int) = (string x) + (string y) |> int
  match nums with
  | [x; y] -> [concat x y; concat y x]
  | n -> failwith $"Got subset of size {List.length n} instead of 2."

let n = 100_000_000
printfn $"Pre-computing prime cache..."
let primes_cache = primes_to n |> Set.ofList
let is_prime n = Set.contains n primes_cache

let primes = primes_to 9999 
let plen = List.length primes

let concat_table_list = Dictionary<int, List<int>>()
let concat_table_self = Dictionary<int, List<int>>()
for i in 0 .. plen-1 do
  let pi = primes[i]
  concat_table_list.Add(pi, new List<int>()) 
  concat_table_self.Add(pi, new List<int>()) 
  concat_table_self[pi].Add(pi)

for i in 0 .. plen-1 do
  let pi = primes[i]
  for j in i+1 .. plen-1 do
    let pj = primes[j]
    let ns = concat_nums [pi; pj]
    if List.forall (fun elem -> is_prime elem = true) ns then concat_table_list[pi].Add(pj); concat_table_list[pj].Add(pi); concat_table_self[pi].Add(pj); concat_table_self[pj].Add(pi) |> ignore

let concat_table_set = Dictionary<int, Set<int>>()
for i in 0 .. plen-1 do
  let pi = primes[i] 
  concat_table_set.Add(pi, Set.ofSeq concat_table_self[pi])

let get_intersection = function
  | s when List.length s = 5 -> s |> List.map (fun i -> concat_table_set[i]) |> Set.intersectMany
  | s -> failwith $"Got subset of size {List.length s} instead of 5."

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

printfn $"Using the first {plen} primes."

let mutable min = 1_000_000_000
for i in 0 .. plen-1 do
  let pi = primes[i]
  let candidates = subsets_of_size_k 4 (Seq.toList concat_table_list[pi]) |> List.map (fun ls -> pi :: ls)
  // printfn $"Candidates: %A{candidates}"
  let filtered = List.filter (fun elem -> Set.count (get_intersection elem) = 5) candidates |> List.map (List.sum)
  if List.length filtered <> 0 then min <- System.Math.Min(List.min filtered,min)
  // printfn $"Filtered: %A{filtered}"

printfn $"Result: {min}"
