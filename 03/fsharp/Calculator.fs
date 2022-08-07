open System

// Instead of FsToolkit.ErrorHandling
module Result =
    let fold errF okF = function
        | Ok v -> okF v
        | Error e -> errF e

type Command =
    | Divide of double * double
    | Sum of double list
    | Average of double list
    | Min of double list
    | Max of double list

let concat (xs: 'a seq) = String.Join (' ', xs)

let parseCommand (x: string) =
    let (|Number|_|) (x: string) =
        let success, number = Double.TryParse x
        if success then Some number else None

    let (|Collect|_|) (|Pattern|_|) (xs: string list) =
        xs |> List.choose (|Pattern|_|) |> function [] -> None | numbers -> Some numbers

    let (|Numbers|_|) = (|Collect|_|) (|Number|_|)

    let isCommand command = [ "sum"; "average"; "min"; "max" ] |> List.contains command

    match x.ToLowerInvariant().Split ([| ' ' |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList with
    | [ "divide"; Number dividend; Number divisor ] -> Divide (dividend, divisor) |> Ok
    | [ "divide"; Number _; x ] -> Error $"Failed to parse divisor '{x}'"
    | [ "divide"; x; Number _ ] -> Error $"Failed to parse dividend '{x}'"
    | [ "divide"; n1; n2 ] -> Error $"Failed to parse divisor '{n1}' and dividend '{n2}'"
    | "divide" :: _ -> Error $"'divide' command requires dividend and divisor specified"

    | "sum" :: Numbers ns -> Sum ns |> Ok
    | "average" :: Numbers ns -> Average ns |> Ok
    | "min" :: Numbers ns -> Min ns |> Ok
    | "max" :: Numbers ns -> Max ns |> Ok

    | [ command ] when isCommand command -> Error $"'{command}' command requires at least one number as an argument"
    | command :: numbers when isCommand command -> Error $"Failed to parse numbers '{concat numbers}'"
    | [] -> Error $"No command specified"
    | command :: _ -> Error $"Unknown command '{command}'"

    | xs -> Error $"Failed to parse line: '{concat xs}'"

type CommandResult = CommandResult of Command * double

let calculate c =
    match c with
    | Divide (_, 0.) -> Error "Divisor cannot be 0"
    | Divide (dividend, divisor) -> dividend / divisor |> Ok
    | Sum ns -> List.sum ns |> Ok
    | Average ns -> List.average ns |> Ok
    | Min ns -> List.min ns |> Ok
    | Max ns -> List.max ns |> Ok
    |> Result.map (fun r -> CommandResult(c, r))

let renderDoubleList c xs r =
    $"the {c} of {concat xs} is {r}"

let renderResult = function
    | CommandResult (Divide (dividend, divisor), r) -> $"{dividend} divided by {divisor} is {r}"
    | CommandResult (Sum ns, r) -> renderDoubleList "sum" ns r
    | CommandResult (Average ns, r) -> renderDoubleList "average" ns r
    | CommandResult (Min ns, r) -> renderDoubleList "minimum" ns r
    | CommandResult (Max ns, r) -> renderDoubleList "maximum" ns r

let proc =
    // parseCommand >> Result.bind calculate >> Result.map renderResult >> function Ok msg -> msg | Error msg -> $"Error: {msg}"
    parseCommand >> Result.bind calculate >> Result.map renderResult >> Result.fold (fun msg -> $"Error: {msg}") id

Seq.initInfinite (fun _ -> Console.ReadLine())
|> Seq.takeWhile (not << String.IsNullOrEmpty)
|> Seq.iter (proc >> Console.WriteLine)