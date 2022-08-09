#load "../common.fs"
open Common

let sieve1 = primes_to 1_000_000
printfn "primes in first 1_000_000 natural numbers: %d" (List.length sieve1)

let sieve2 = primes_to 2_000_000
printfn "primes in first 2_000_000 natural numbers: %d" (List.length sieve2)

printfn "6th prime: %d" (sieve2.Item 5)
printfn "10_001st prime: %d" (sieve2.Item 10_000)