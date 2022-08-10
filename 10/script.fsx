#load "../common.fs"
open Common

let (sieve1: uint64 list) = primes_to 2_000_000 |> List.map (fun x -> uint64 x)
printfn "highest value prime in first 2_000_000 natural numbers: %d" (List.rev sieve1 |> List.head)
printfn "sum: %d" (List.sum sieve1)