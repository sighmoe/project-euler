let str_of_num n =
  let decode = function
    | 0 -> "zero"
    | 1 -> "one"
    | 2 -> "two"
    | 3 -> "three"
    | 4 -> "four"
    | 5 -> "five"
    | 6 -> "six"
    | 7 -> "seven"
    | 8 -> "eight"
    | 9 -> "nine"
    | 10 -> "ten"
    | 11 -> "eleven"
    | 12 -> "twelve"
    | 13 -> "thirteen"
    | 14 -> "fourteen"
    | 15 -> "fifteen"
    | 16 -> "sixteen"
    | 17 -> "seventeen"
    | 18 -> "eighteen"
    | 19 -> "nineteen"
    | 20 -> "twenty"
    | 30 -> "thirty"
    | 40 -> "forty"
    | 50 -> "fifty"
    | 60 -> "sixty"
    | 70 -> "seventy"
    | 80 -> "eighty"
    | 90 -> "ninety"
    | 100 -> "onehundred"
    | 200 -> "twohundred"
    | 300 -> "threehundred"
    | 400 -> "fourhundred"
    | 500 -> "fivehundred"
    | 600 -> "sixhundred"
    | 700 -> "sevenhundred"
    | 800 -> "eighthundred"
    | 900 -> "ninehundred"
    | 1000 -> "onethousand"
    | k -> failwith $"got unexpected input {k}"

  let rec find_closest k x =
    match k-x with
    | d when d >= 0 -> x
    | _ -> find_closest k (if x > 100 then x-100 else x-10)

  let insert_and lst = 
    match List.length lst with
    | 3 -> (List.head lst) :: "and" :: (List.tail lst)
    | 2 when (List.head lst |> fun (s: string) -> s.Contains("hundred")) -> (List.head lst) :: "and" :: (List.tail lst)
    | _ -> lst

  let rec aux acc = function
    | 0 -> List.rev acc |> insert_and |> String.concat ""
    | k when k < 20 -> decode k |> fun x -> x :: acc |> List.rev |> insert_and |> String.concat "" 
    | k -> let closest = (find_closest k 1000) in aux (closest |> decode |> fun x -> x :: acc) (k-closest)
  in aux List.empty n

printfn $"str_of_num 1: {str_of_num 1}"
printfn $"str_of_num 21: {str_of_num 21}"
printfn $"str_of_num 111: {str_of_num 111}"
printfn $"str_of_num 987: {str_of_num 987}"

printfn $"str_of_num 342: {str_of_num 342} with {str_of_num 342 |> String.length} characters."
printfn $"str_of_num 115: {str_of_num 115} with {str_of_num 115 |> String.length} characters."


[for i in 1 .. 5 -> i] 
|> List.map str_of_num 
|> List.map String.length 
|> List.sum 
|> printfn "Total characters in 1 .. 5: %d"

[for i in 1 .. 1000 -> i] 
|> List.map str_of_num 
|> List.map String.length 
|> List.sum 
|> printfn "Total characters in 1 .. 1000: %d"