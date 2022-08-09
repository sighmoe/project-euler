let is_multiple x y = x % y = 0
let ans = seq { 1 .. 999 } |> Seq.filter (fun n -> (is_multiple n 3) || (is_multiple n 5)) |> Seq.sum
printfn "%d" ans