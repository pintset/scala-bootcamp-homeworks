module Protocol

open System

type NewGame =
    { Min: int
      Max: int }

type Guess =
    { Id: Guid
      Number: int }

type GameResult =
    | YouWon
    | GameOver
    | Greater
    | Lower
    | GameNotFound