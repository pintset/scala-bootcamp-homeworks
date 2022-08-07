open System

// https://en.wikipedia.org/wiki/Greatest_common_divisor
let gcd a b =
    let rec inner a b =
        let m = a % b
        if m = 0 then b else inner b m
    if a > b then inner a b else inner b a

// https://en.wikipedia.org/wiki/Lowest_common_denominator
let lcm a b = a * b / (gcd a b)

let readNatNum p =
    printf "Enter %s: " p
    Console.ReadLine () |> Int32.TryParse |> function true, v when v > 0 -> Some v | _ -> None

(readNatNum "a", readNatNum "b")
||> Option.map2 (fun a b -> $"gcd({a}, {b}) = %A{gcd a b} and lcm({a}, {b}) = %A{lcm a b}")
|>  Option.defaultValue "Failed to parse parameters. Natural numbers are expected"
|>  Console.WriteLine