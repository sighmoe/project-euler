let sum_square_diff n =
  let mutable res = 0
  for j in 1 .. n do
    let mutable sum = 0
    for i in 1 .. j-1 do
      sum <- sum + i*j
    res <- res + (2*sum) 
  res

printfn "Answer for input 10: %d" (sum_square_diff 10)
printfn "Answer for input 100: %d" (sum_square_diff 100)


