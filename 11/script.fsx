#load "../common.fs"
open Common

open System.Collections.Generic

let grid = readLines "grid.txt" |> Seq.map (fun (line: string) -> line.Split ' ' |> Array.map int) |> Seq.toArray
let print_grid (grid: int[][]) =
  for i in 0 .. 19 do
    for j in 0 .. 19 do
      printf "%d " grid[i].[j]
    printfn ""
let products (grid: int[][]) = 
  let ps = new List<int>()
  for i in 0 .. grid.Length-1 do
    for j in 0 .. grid[0].Length-1 do
      if i <= grid.Length-4 then ps.Add(grid[i][j] * grid[i+1][j] * grid[i+2][j] * grid[i+3][j]) // right
      if j <= grid.Length-4 then ps.Add(grid[i][j] * grid[i][j+1] * grid[i][j+2] * grid[i][j+3]) // down
      if i <= grid.Length-4 && j <= grid.Length-4 then ps.Add(grid[i][j] * grid[i+1][j+1] * grid[i+2][j+2] * grid[i+3][j+3]) // diag down right
      if i <= grid.Length-4 && j >= 3 then ps.Add(grid[i][j] * grid[i+1][j-1] * grid[i+2][j-2] * grid[i+3][j-3]) // diag down left
  seq ps

printfn "Largest product: %d" (products grid |> Seq.max)