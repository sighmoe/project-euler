#load "lookup.fsx"
open Lookup

open System

type Result = P1Win | P2Win | Draw 
type Summary = int * int * Result
type Rank = int
type Suit = Spade | Club | Heart | Diamond | Unknown
type Card = Rank * Suit 
type Hand = int * int * int * int * int 

let primes = [|2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41|]

let parse_suit = function
  | 'S' -> Spade
  | 'C' -> Club
  | 'H' -> Heart
  | 'D' -> Diamond
  | c -> printfn "Unexpected suit %c" c; Unknown 

let parse_rank = function
  | '2' -> 0
  | '3' -> 1
  | '4' -> 2
  | '5' -> 3
  | '6' -> 4
  | '7' -> 5
  | '8' -> 6
  | '9' -> 7
  | 'T' -> 8
  | 'J' -> 9 
  | 'Q' -> 10
  | 'K' -> 11
  | 'A' -> 12
  | c -> printfn "Unexpected rank %c" c; int c

let parse_card (s: string) = Card (parse_rank s[0],parse_suit s[1]) 
let parse_cards (line: string) = line.Split ' ' |> (fun s -> (Array.take 5 s |> Array.toSeq |> Seq.map parse_card, Array.skip 5 s |> Array.toSeq |> Seq.map parse_card))

let get_prime_bits (rank: Rank) = primes[rank]
let get_rank_suit_bits (rank: Rank) (suit: Suit) = 
  match suit with
  | Spade -> 1 <<< 4 ||| rank
  | Heart -> (1 <<< 1) <<< 4 ||| rank
  | Diamond -> (1 <<< 2) <<< 4 ||| rank
  | Club -> (1 <<< 3) <<< 4 ||| rank
  | Unknown -> printfn "Trying to get bits from unknown suit"; 0
let get_rank_bit (rank: Rank): int32 = 1 <<< rank 

let int_of_card (rank, suit): int32 = (get_rank_bit rank <<< 16) ||| (get_rank_suit_bits rank suit <<< 8) ||| get_prime_bits rank
let hand_of_cards (cards: seq<Card>): Hand = 
  match cards |> Seq.map int_of_card |> Seq.toList with
  | [c1; c2; c3; c4; c5] -> (c1,c2,c3,c4,c5)
  | _ -> printfn "Hand did not contain a sequence of exactly 5 cards"; (0,0,0,0,0) 


let format_bits (s: string) = 
  let mutable ss = s
  let mutable i = 8
  while i <= s.Length do
    ss <- ss.Insert(i, " ")
    i <- i+9
  ss
let print_bits (i: int32) = Convert.ToString(i, 2).PadLeft(32, '0') |> format_bits

let test_to_bits () = (
  printfn "Int of card King of Diamonds: %s" (int_of_card (11, Diamond) |> print_bits);
  printfn "Int of card Five of Spades: %s" (int_of_card (3, Spade) |> print_bits);
  printfn "Int of card Jack of Clubs: %s" (int_of_card (9, Club) |> print_bits)
)

let has_flush (c1,c2,c3,c4,c5) = (c1 &&& c2 &&& c3 &&& c4 &&& c5 &&& 0xF000) > 0
let q_index (c1,c2,c3,c4,c5) = (c1 ||| c2 ||| c3 ||| c4 ||| c5) >>> 16

let find_fast q = 
  let mutable u = uint q
  let mutable a = uint 0
  let mutable b = uint 0
  
  u <- u + (uint 0xE91AAA35);
  u <- u ^^^ (u >>> 16)
  u <- u + (u <<< 8)
  u <- u ^^^ (u >>> 4)
  b <- (u >>> 8) &&& (uint 0x1FF)
  a <- (u + (u <<< 2)) >>> 19
  a ^^^ (uint hash_adjust[int b])

let eval_hand (hand: Hand) =
  let qi = q_index hand
  if has_flush hand then flushes[qi]
  elif unique5[qi] <> 0 then unique5[qi]
  else
    let (c1,c2,c3,c4,c5) = hand
    let q = (c1 &&& 0xFF) * (c2 &&& 0xFF) * (c3 &&& 0xFF) * (c4 &&& 0xFF) * (c5 &&& 0xFF) 
    let qi = find_fast q
    hash_values[int qi]

let eval (s: string): Summary =
  let (cs1, cs2): (seq<Card> * seq<Card>) = parse_cards s
  let (h1, h2) = (hand_of_cards cs1, hand_of_cards cs2)
  let (v1, v2) = (eval_hand h1, eval_hand h2)
  if v1 < v2 then (v1, v2, P1Win)
  elif v2 < v1 then (v1, v2, P2Win)
  else (v1, v2, Draw)

let test_eval_hand () = (
  let flush1 = "AD KD QD JD 9D 2D 3D 4D 5D 7D"
  printfn "Winner of flush1 test: %A" (eval flush1) 

  let flush2 = "2D 4D 3D 6D 5D AD QD TD JD KD"
  printfn "Winner of flush2 test: %A" (eval flush2) 
)

test_to_bits ()
test_eval_hand ()

