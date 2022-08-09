let prime_factors (N: int64) =
  let rec prime_factors (n: int64) (p: int64) s =
    match N >= p * p with
    | false -> s
    | _ -> match n % p = 0L with
           | true -> prime_factors (n / p) p (p :: s)
           | _ -> prime_factors n (p+1L) s
  in prime_factors N 2 List.empty

let factors = prime_factors 600851475143L 
printfn "factors %A" factors
let factor = factors |> List.max
printfn "%d" factor