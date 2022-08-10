open System.Collections.Generic

let gen_triplet sum= 
  let triplets = new List<int*int*int>()
  for c in 1 .. sum do
    for b in 1 .. sum - c do
      if sum-c-b < b && b < c then triplets.Add((sum-c-b,b,c))
  triplets |> seq

let is_pythagorean_triplet (a,b,c) = a*a+b*b = c*c

let pythag12 = gen_triplet 12 |> Seq.filter is_pythagorean_triplet
printfn "Pythaogren triplets that sum to 12 %A" pythag12

let pythag1000 = gen_triplet 1000 |> Seq.filter is_pythagorean_triplet
printfn "Pythaogren triplets that sum to 1000 %A" pythag1000
printfn "Product: %d" (Seq.head pythag1000 |> (fun (a,b,c) -> a*b*c))