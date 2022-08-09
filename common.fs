module Common

open System.Collections.Generic

let sqrti n = sqrt (float n) |> int32

let filterComposites (sieve: bool array) =
  let primes = new List<int32>()
  for i in 2 .. (sieve.Length-1) do
    if sieve[i] then primes.Add(i)
  Seq.toList primes

let primes_to n = 
  let sieve = [|for _ in 0 .. (n+1) -> true|]
  for i in 2 .. (sqrti n) do
    for j in i*i .. i .. n do
      sieve[j] <- false
  filterComposites sieve


