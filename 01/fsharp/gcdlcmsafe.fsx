module Types =
    type NaturalNumber = private NaturalNumber of int

    module NaturalNumber =
        let fromInt n = if n > 0 then NaturalNumber n |> Some else None
        let toInt (NaturalNumber n) = n

        let gcd (NaturalNumber a) (NaturalNumber b) =
            let rec inner a b =
                let m = a % b
                if m = 0 then (NaturalNumber b) else inner b m
            if a > b then inner a b else inner b a

        let lcm a b =
            let gcd = gcd a b |> toInt
            let a, b = a |> toInt, b |> toInt
            NaturalNumber <| a * b / gcd

open System
open Types

// Execute (without input parse check here)
let input message =
    printf message
    Console.ReadLine () |> Int32.Parse

let calc f a b =
    let toTupleOpt = function
    | Some a, Some b -> Some (a, b)
    | _ -> None

    (NaturalNumber.fromInt a, NaturalNumber.fromInt b)
    |> toTupleOpt 
    |> Option.map (fun (a, b) -> f a b)

let gcd = calc NaturalNumber.gcd
let lcm = calc NaturalNumber.lcm

let a = input "Enter a: "
let b = input "Enter b: "

printfn $"gcd({a}, {b}) = %A{gcd a b} and lcm({a}, {b}) = %A{lcm a b}"