let products = 
  seq {
     for i in 100 .. 999 do 
      for j in 100 .. 999 -> i * j 
  }

printfn "products %A" products
let is_palindrome s = s = (Seq.rev s |> Seq.map string |> String.concat "")

let palProds= products |> Seq.map (fun i -> i.ToString()) |> Seq.filter is_palindrome |> Seq.map int32
printfn "palindromic products: %A" palProds
printfn "%d" (Seq.max palProds)