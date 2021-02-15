module Types =
    type Suit = Clubs | Diamonds | Hearts | Spades

    type Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace

    type Card = Card of Rank * Suit

    type Err =
        | InvalidNumberOfCards of actual: int * expected: int
        | DuplicateCardsDetected of Card list
        | UnsupportedHandSize of int

    type Card2 = private Card2 of Card list
    type Card4 = private Card4 of Card list
    type Card5 = private Card5 of Card list

    module CardN =
        let create f n cards =
            match List.length cards with
            | len when len <> n -> InvalidNumberOfCards (actual = len, expected = n) |> Error
            | _ when (List.distinct cards).Length <> n ->
                cards |> List.countBy id |> List.filter (fun (_, count) -> count > 1) |> List.map fst |> DuplicateCardsDetected |> Error
            | _ -> cards |> f |> Ok

    module Card2 =
        let create = CardN.create Card2 2
        let apply f (Card2 cards) = f cards

    module Card4 =
        let create = CardN.create Card4 4
        let apply f (Card4 cards) = f cards

    module Card5 =
        let create = CardN.create Card5 5
        let apply f (Card5 cards) = f cards

    type Hand =
        | Texas of Card2
        | Omaha of Card4

    module Hand =
        let create cards =
            match List.length cards with
            | 2 -> Card2.create cards |> Result.map Texas
            | 4 -> Card4.create cards |> Result.map Omaha
            | x -> UnsupportedHandSize x |> Error

    type Board = Board of Card5

    module Board =
        let create = Card5.create >> Result.map Board
        let apply f (Board board) = Card5.apply f board

module Combinations =
    open Types

    let private choose n k =
        let rec inner lo = function
            | 0 -> [ [] ]
            | i -> [ for j = lo to (Array.length n) - 1 do
                        for ks in inner (j + 1) (i - 1) do
                            yield n.[j] :: ks ]
        inner 0 k

    let private texasHoldem board hand = [
        yield board

        let board = board |> List.toArray
        let singles = choose board 4

        for card in hand do
            for comb in singles do
                yield card :: comb

        let doubles = choose board 3
        for comb in doubles do
            yield hand @ comb
    ]

    let private omahaHoldem board hand = [
        let board = board |> List.toArray
        let hand = hand |> List.toArray

        let hands = choose hand 2
        let doubles = choose board 3

        for hand in hands do
            for comb in doubles do
                yield hand @ comb
    ]

    let get board hand = board |> Board.apply (fun board ->
        match hand with
        | Texas cards -> cards |> Card2.apply (texasHoldem board)
        | Omaha cards -> cards |> Card4.apply (omahaHoldem board))

module Evaluate =
    open System
    open Types

    let private ranks = [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ]

    let private rank' (Card (rank, _)) = rank
    let private suit' (Card (_, suit)) = suit

    let private (|HighCard|) hand =
        hand |> List.map rank' |> List.sortDescending

    let private (|Group|) hand =
        let swap (x, y) = (y, x)
        hand |> List.countBy rank' |> List.map swap |> List.sortDescending |> List.unzip

    let private (|Straight|_|) =
        let straights = Ace :: ranks |> List.windowed 5 |> List.map List.rev
        let equalTo l1 l2 = (l2 |> List.except l1).Length = 0
        fun hand ->
            let handRanks = hand |> List.map rank'
            let test = straights |> List.tryFind (equalTo handRanks)
            match test with
            | Some x -> Some x
            | None -> None

    let private (|Flush|_|) hand =
        hand |> List.countBy suit' |> List.tryExactlyOne |> Option.map (fun _ -> hand |> List.map rank' |> List.sortDescending)

    let private hexMap = ranks |> List.mapi (fun i r -> r, i.ToString("X").[0]) |> Map.ofList
    let private toInt (s: string) = Convert.ToInt32 (s, 16)

    let private calc (prefix: char) ranks =
        [| prefix; for rank in ranks do hexMap.[rank] |] |> String |> toInt

    let rank = function
        | Flush _ & Straight ranks -> calc '8' ranks
        | Group ([ 4; 1 ], [ r; k ]) -> calc '7' [ r; r; r; r; k ]
        | Group ([ 3; 2 ], [ r1; r2 ]) -> calc '6' [ r1; r1; r1; r2; r2 ]
        | Flush ranks -> calc '5' ranks
        | Straight ranks -> calc '4' ranks
        | Group ([ 3; 1; 1 ], [ r; k1; k2 ]) -> calc '3' [ r; r; r; k1; k2 ]
        | Group ([ 2; 2; 1 ], [ r1; r2; k ]) -> calc '2' [ r1; r1; r2; r2; k ]
        | Group ([ 2; 1; 1; 1 ], [ r; k1; k2; k3 ]) -> calc '1' [ r; r; k1; k2; k3 ]
        | HighCard ranks -> calc '0' ranks

open Types

let sortHands board hands =
    let evaluateRank = Combinations.get board >> List.map Evaluate.rank >> List.max
    hands |> List.map (fun h -> h, h |> evaluateRank) |> List.sortBy snd |> List.map fst
    // hands |> List.map (fun h -> h, h |> evaluateRank) |> List.sortBy snd |> List.map (fun (hand, rank) ->
    //     match rank with
    //     | rank when rank >= 0x800000 -> "StraightFlush"
    //     | rank when rank >= 0x700000 -> "FourOfAKind"
    //     | rank when rank >= 0x600000 -> "FullHouse"
    //     | rank when rank >= 0x500000 -> "Flush"
    //     | rank when rank >= 0x400000 -> "Straight"
    //     | rank when rank >= 0x300000 -> "ThreeOfAKind"
    //     | rank when rank >= 0x200000 -> "TwoPairs"
    //     | rank when rank >= 0x100000 -> "Pair"
    //     | _ -> "Highcard"
    //     , hand)

let traverseResultM f list =
    let (>>=) x f = Result.bind f x
    let retn = Ok

    let cons head tail = head :: tail

    let initState = retn []
    let folder head tail =
        f head >>= (fun h ->
        tail >>= (fun t ->
        retn (cons h t) ))

    List.foldBack folder list initState

let sequenceResultM list = traverseResultM id list

// 4cKs4h8s7s Ad4s Ac4d As9s KhKd 5d6d
let board = Board.create [
    Card (Four, Clubs)
    Card (King, Spades)
    Card (Four, Hearts)
    Card (Eight, Spades)
    Card (Seven, Spades)
]

let hands = sequenceResultM [
    Hand.create [ Card (Ace, Diamonds); Card (Four, Spades) ]
    Hand.create [ Card (Ace, Clubs); Card (Four, Diamonds) ]
    Hand.create [ Card (Ace, Spades); Card (Nine, Spades) ]
    Hand.create [ Card (King, Hearts); Card (King, Diamonds) ]
    Hand.create [ Card (Five, Diamonds); Card (Six, Diamonds) ]
]

type ResultBuilder () =
    member __.Bind(m, f) = Result.bind f m
    member __.Return(x) = Ok x

let result = ResultBuilder ()

let testCE = result {
    let! board = board
    let! hands = hands

    return sortHands board hands
}

// OR
module Result =
    let apply fResult xResult =
        fResult |> Result.bind (fun f -> xResult |> Result.map f)

let testApply =
    let (<!>) = Result.map
    let (<*>) = Result.apply

    sortHands <!> board <*> hands

printfn "%A" testCE
printfn "%A" (testCE = testApply)