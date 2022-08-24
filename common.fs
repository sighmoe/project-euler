module Common

open System.Collections.Generic

let readLines filePath = System.IO.File.ReadLines(filePath)

let sqrti n = sqrt (float n) |> int
let sqrtui n = sqrt (float n) |> uint64

let primes_to n = 
  let sieve = [|for _ in 0 .. n -> true|]

  for i in 2 .. (sqrti n) do
    for j in i*i .. i .. n do
      sieve[j] <- false

  let filterComposites (sieve: bool array) =
    let primes = new List<int>()

    for i in 2 .. (sieve.Length-1) do
      if sieve[i] then primes.Add(i)

    Seq.toList primes
  in filterComposites sieve

let subsets_of_size_k (k: int) (s: int list) =
  let arr = List.toArray s
  let n = Array.length arr
  let subsets = new List<int list>()
  let rec aux (acc: int list) (idx: int) = function
    | 0 -> subsets.Add(acc) 
    | j -> for i in idx .. n-1 do aux (arr[i] :: acc) (i+1) (j-1)
  in aux List.empty 0 k
  Seq.toList subsets

let get_divisors (n: uint64) =
  let divisors = new List<uint64>()
  for i in 1UL .. sqrtui n do
    if n % i = 0UL then
      divisors.Add(i)
      divisors.Add(n/i)
  seq divisors