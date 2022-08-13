#load "../common.fs"
open Common

let collatz (n: uint64) =
  let rec collatz acc = function
    | 1UL -> (n,acc)
    | n when n % 2UL = 0UL -> collatz (acc+1UL) (n/2UL)
    | n -> collatz (acc+1UL) (3UL*n+1UL)
  in collatz 0UL n

[for i in 1UL .. 999999UL -> i] 
|> List.map collatz 
|> List.maxBy (fun (_,acc) -> acc)
|> fun (n,acc) -> printfn $"%d{n} has longest collatz sequence with length of %d{acc}"