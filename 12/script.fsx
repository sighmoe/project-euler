#load "../common.fs"
open Common

let triangle_num (n: uint64) = [for i in 1UL .. n -> i] |> List.sum

let solve (n: uint64) =
  [for i in 1UL .. n -> i] 
  |> List.map (fun i -> triangle_num i) 
  |> List.map (fun i -> get_divisors i |> Seq.length |> fun j -> (i,j)) 
  |> List.iter (fun (i,j) -> if j > 500 then printfn $"Triangle number %d{i} has %d{j} divisors")

solve 50000UL