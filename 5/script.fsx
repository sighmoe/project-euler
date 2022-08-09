open System.Collections.Generic

let smallest_divisible_by_first_n n =
  let factors = new List<int32>()
  for i in 2 .. n do
    let mutable j = i
    factors |> (Seq.toList) |> List.iter (fun x -> if j % x = 0 then j <- j / x) 
    factors.Add(j)

  printf "Factors: " 
  Seq.iter (fun x -> printf "%d " x) factors
  printfn ""

  List.reduce (fun acc elem -> acc*elem) (Seq.toList factors)

printfn "Answer for %d: %d" 10 (smallest_divisible_by_first_n 10)
printfn "Answer for %d: %d" 20 (smallest_divisible_by_first_n 20)




