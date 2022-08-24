#load "../common.fs"
open Common

let mutable bigNum = bigint 1
for _ in 1 .. 1000 do
  bigNum <- bigNum * bigint 2 

printfn $"BigNum: {bigNum}"

let mutable ans = bigint 0
while bigNum > bigint 0 do
  ans <- ans + (bigNum % bigint 10)
  bigNum <- bigNum / bigint 10

printfn $"Answer: {ans}"