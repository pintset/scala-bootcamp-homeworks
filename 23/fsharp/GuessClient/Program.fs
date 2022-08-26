open System
open Hopac
open Hopac.Infixes
open HttpFs.Client
open Newtonsoft.Json

open Protocol

let url path = "http://localhost:5000/" + path

module Request =
    let body x = x |> JsonConvert.SerializeObject |> Request.bodyString

module Response =
    let decode<'a> = Job.map JsonConvert.DeserializeObject<'a>

module Client =
    let expect method body url =
        Request.createUrl method url
        |> Request.body body
        |> Request.responseAsString
        |> Response.decode

let startJob min max : Job<Guid> =
    url "start" |> Client.expect Post { Min = min; Max = max }

let rec guessJob min max (guid: Guid) =
    let number = min + (max - min) / 2
    url "guess"
    |> Client.expect Post { Id = guid; Number = number }
    >>= function
        | YouWon -> number |> string |> Job.result
        | Greater -> guessJob min number guid
        | Lower -> guessJob number max guid
        | GameOver -> "Game over" |> Job.result
        | GameNotFound -> "Game not found" |> Job.result

[<EntryPoint>]
let main _ =
    let min = 10
    let max = 100

    let clientJob =
        startJob min max >>= guessJob min max

    clientJob >>- printfn "%s" |> run

    //let measuredJob (j: Job<'a>) : Job<'a> = job {
    //    let startTime = DateTime.Now
    //    let! results = j
    //    // let runTime = "hh:mm:ss.fff" |> (DateTime.Now - startTime).ToString
    //    let runTime = @"hh\:mm\:ss\.fff" |> (DateTime.Now - startTime).ToString
    //    printfn "Run time: %s" runTime
    //    return results
    //}

    //let gamesWon =
    //    Seq.filter (fun x -> x <> "Game over") >> Seq.length >> printfn "\tGames won: %i"

    //for _ in [1..100] do
    //    Seq.init 1000 (fun _ -> clientJob) |> Job.conCollect |> measuredJob |> run |> gamesWon
    //    // Seq.init 1000 (fun _ -> clientJob) |> Job.conIgnore |> measuredJob |> run

    Console.ReadLine() |> ignore

    0