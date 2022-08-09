let fib n =
  let rec fib x y s =
    if x <= n && y <= n then fib y (x+y) ((x+y) :: s) else (1 :: 2 :: s)
  in fib 1 2 List.empty

let even = fib 4000000 |> List.filter (fun x -> x % 2 = 0) |> List.sum
printfn "%d" even