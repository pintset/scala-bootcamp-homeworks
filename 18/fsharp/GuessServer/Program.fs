open System

open Giraffe
open Saturn

open Atom
open Protocol

type Game =
    { Number: int
      AttemptsLeft: int }

let startHandler ref (rnd: Random) newGame : HttpHandler =
    fun next ctx ->
        let id = Guid.NewGuid()
        swap ref <| Map.add id { Number = rnd.Next(newGame.Min, newGame.Max); AttemptsLeft = 5 }
        json id next ctx

let guessHandler ref guess : HttpHandler =
    fun next ctx ->
        let result =
            (!ref) |> Map.tryFind guess.Id
            |> Option.map (fun g ->
                let game = { g with AttemptsLeft = g.AttemptsLeft - 1 }

                swap ref <|
                    if game.AttemptsLeft = 0 || guess.Number = game.Number then
                        Map.remove guess.Id
                    else
                        Map.add guess.Id game

                if guess.Number = game.Number then YouWon
                elif game.AttemptsLeft = 0 then GameOver
                elif guess.Number > game.Number then Greater
                else Lower)
            |> Option.defaultValue GameNotFound

        json result next ctx

let webApp =
    let ref = atom Map.empty
    let rnd = Random()
    choose [
        route "/guess" >=> bindJson<Guess>   (guessHandler ref)
        route "/start" >=> bindJson<NewGame> (startHandler ref rnd)

        RequestErrors.NOT_FOUND "Invalid path"
    ]

let app = application {
    use_router webApp
}

run app
