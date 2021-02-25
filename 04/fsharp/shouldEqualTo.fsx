open System

let shouldEqualTo x y =
    if x = y then (ConsoleColor.Green, "Pass") else (ConsoleColor.Red, "Failed")
    ||> fun color message ->
        Console.ForegroundColor <- color
        Console.WriteLine message
        Console.ResetColor()