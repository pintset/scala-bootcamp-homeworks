open System

// Classic
let gcd a b =
    let rec inner a b =
        let m = a % b
        if m = 0 then b else inner b m
    if a > b then inner a b else inner b a

let lcm a b = a * b / (gcd a b)

// Opt
let toTupleOpt = function
    | Some a, Some b -> Some (a, b)
    | _ -> None

let toNatural x = if x > 0 then Some x else None

let gcdOpt a b =
    let rec inner a b =
        let m = a % b
        if m = 0 then b else inner b m
    (toNatural a, toNatural b) |> toTupleOpt |> Option.map (fun (a, b) -> if a > b then inner a b else inner b a)

let lcmOpt a b =
    gcdOpt a b |> Option.map (fun gcd -> a * b / gcd)

// Execute
let run fGcd fLcm =
    let input message =
        printf message
        Console.ReadLine () |> Int32.TryParse |> function true, v -> Some v | _ -> None

    (input "Enter a: ", input "Enter b: ")
    |> toTupleOpt
    |> Option.map (fun (a, b)-> $"gcd({a}, {b}) = %A{fGcd a b} and lcm({a}, {b}) = %A{fLcm a b}")
    |> Option.defaultValue "Unable to parse parameters"
    |> Console.WriteLine

// run gcd lcm
run gcdOpt lcmOpt