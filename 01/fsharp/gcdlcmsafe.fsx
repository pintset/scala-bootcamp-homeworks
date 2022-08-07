open System

module Types =
    type NaturalNumber = private NaturalNumber of int

    module NaturalNumber =
        let parse (s: String) = s |> Int32.TryParse |> function true, v when v > 0 -> Some <| NaturalNumber v | _ -> None 
        let (<*>) f (NaturalNumber n) = f n

        module private Core =
            let gcd a b =
                let rec inner a b =
                    let m = a % b
                    if m = 0 then b else inner b m
                if a > b then inner a b else inner b a

            let lcm a b = a * b / (gcd a b)

        let gcd a b = Core.gcd <*> a <*> b
        let lcm a b = Core.lcm <*> a <*> b

open Types

let readNatNum p =
    printf "Enter %s: " p
    Console.ReadLine () |> NaturalNumber.parse

let calc a b =
    let (<*>) = NaturalNumber.(<*>)
    let result a b gcd lcm = $"gcd({a}, {b}) = {gcd} and lcm({a}, {b}) = {lcm}"
    result <*> a <*> b <| NaturalNumber.gcd a b <| NaturalNumber.lcm a b

(readNatNum "a", readNatNum "b")
||> Option.map2 calc
|>  Option.defaultValue "Failed to parse parameters. Natural numbers are expected"
|>  Console.WriteLine