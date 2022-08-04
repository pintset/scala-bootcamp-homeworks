#load "shouldEqualTo.fsx"
open ShouldEqualTo

// Exercise. Write a function that checks if all values in a `List` are equal.
// Think about what you think your function should return if `list` is empty, and why.
let rec allEqual = function
    | [] | [ _ ] -> true
    | x1 :: x2 :: t when x1 = x2 -> allEqual t
    | _ -> false

[ "a" ] |> allEqual |> shouldEqualTo true
[ "a"; "a"; "b"; "a" ] |> allEqual |> shouldEqualTo false
[] |> allEqual |> shouldEqualTo true
[ 1 ] |> allEqual |> shouldEqualTo true
[ 1; 1 ] |> allEqual |> shouldEqualTo true
[ 1; 1; 2; 1; 1 ] |> allEqual |> shouldEqualTo false
[ 1; 1 ] |> allEqual |> shouldEqualTo true

///////
let vegetableWeights = Map.ofList [
    "pumpkins", 10
    "cucumbers", 20
    "olives", 2
]

let vegetablePrices = Map.ofList [
    "tomatoes", 4
    "peppers", 5
    "olives", 17
]

let vegetableAmounts = Map.ofList [
    "tomatoes", 17
    "peppers", 234
    "olives", 32
    "cucumbers", 323
]

// Exercise. Calculate the total cost of all vegetables, taking vegetable amounts (in units) from
// `vegetableAmounts` and prices per unit from `vegetablePrices`. Assume the price is 10 if not available
// in `vegetablePrices`.
let totalVegetableCost =
    let getPrice v = vegetablePrices |> Map.tryFind v |> Option.defaultValue 10
    vegetableAmounts |> Map.toList |> List.sumBy (fun (v, a) -> a * (getPrice v))

totalVegetableCost |> shouldEqualTo 5012

// Exercise. Given the vegetable weights (per 1 unit of vegetable) in `vegetableWeights` and vegetable
// amounts (in units) in `vegetableAmounts`, calculate the total weight per type of vegetable, if known.
//
// For example, the total weight of "olives" is 2 * 32 == 64.
let totalVegetableWeights =
    let getWeight v = vegetableWeights |> Map.tryFind v
    vegetableAmounts |> Map.toList |> List.choose (fun (v, a) -> getWeight v |> Option.map (fun w -> v, w * a))

totalVegetableWeights |> shouldEqualTo [ ("cucumbers", 6460); ("olives", 64) ]

let set = Set.ofList

// Exercise: Return a set with all subsets of the provided set `set` with `n` elements
// For example, `allSubsetsOfSizeN(Set(1, 2, 3), 2) == Set(Set(1, 2), Set(2, 3), Set(1, 3))`.
// Hints for implementation:
//   - Handle the trivial case where `n == 1`.
//   - For other `n`, for each `set` element `elem`, generate all subsets of size `n - 1` from the set
//     that don't include `elem`, and add `elem` to them.
let allSubsetsOfSizeN set n =
    // n choose k from Poker task implementation converted to Set
    let arr = Set.toArray set
    let rec inner lo = function
        | 0 -> [ [] ]
        | i -> [ for j = lo to (Array.length arr) - 1 do
                    for ks in inner (j + 1) (i - 1) do
                        yield arr.[j] :: ks ]
    inner 0 n |> List.map Set.ofList |> Set.ofList

allSubsetsOfSizeN (Set.ofList [ 1; 2; 3 ]) 2 |> shouldEqualTo (set [
    set [ 1; 2 ]
    set [ 2; 3 ]
    set [ 1; 3 ]
])

// Homework
//
// Implement a special sort which sorts the keys of a map (K) according to their associated
// values (V).
//
// In case of "ties" (equal values) it should group these keys K into Set-s in the results.
//
// The input is a map where keys (K) are the values to be sorted and values are their associated numeric
// values.
//
// The output is a list (in ascending order according to the associated `Int` values) of tuples of `Set`-s
// with values from K, and the associated value V for these values in the `Set`.
//
// For example:
//
// Input `Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)` should result in
// output `List(Set("e") -> 0, Set("a", "d") -> 1, Set("b", "f", "g") -> 2, Set("c") -> 4)`.
let sortConsideringEqualValues map =
    map |> Map.toList |> List.groupBy snd |> List.map (fun (c, xs) -> xs |> List.map fst |> Set.ofList, c) |> List.sortBy snd

let input = Map.ofList [
    'f', 2
    'b', 2
    'c', 4
    'd', 1
    'a', 1
    'g', 2
    'e', 0
]
let expected = [
    set [ 'e' ], 0
    set [ 'a'; 'd' ], 1
    set [ 'b'; 'f'; 'g' ], 2
    set [ 'c' ], 4
]
input |> sortConsideringEqualValues |> shouldEqualTo expected

let values = [ "a1"; "a2"; "b1"; "c1"; "c2"; "d1" ] |> List.map (fun x -> x, x |> Seq.head |> int) |> Map.ofList
values |> sortConsideringEqualValues |> shouldEqualTo [
    set [ "a1"; "a2" ], int 'a'
    set [ "b1" ], int 'b'
    set [ "c1"; "c2" ], int 'c'
    set [ "d1" ], int 'd'
]