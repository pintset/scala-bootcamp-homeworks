# Scala Bootcamp 2021 Homeworks

This repo contains homeworks for Scala Bootcamp 2021

[![Scala CI](https://github.com/pintset/scala-bootcamp-homeworks/actions/workflows/bootcamp.yml/badge.svg)](https://github.com/pintset/scala-bootcamp-homeworks/actions/workflows/bootcamp.yml)

The schedule can be found here: https://github.com/evolution-gaming/scala-bootcamp/blob/bfb1594afa73909f486e1f59f918a6186856b3e7/Schedule.md

## Contents

| Number/Url | Lecture Topic                                                                                      | Homework Title                                        |
|------------|----------------------------------------------------------------------------------------------------|-------------------------------------------------------|
| [01](01)   | Introduction and basic Scala Syntax (types, functions, parametric polymorphism)                    | Least Common Multiple & Greatest Common Division      |
| [02](02)   | Classes & Traits and some Control Structures: if-else, pattern matching                            | Areas and Shapes                                      |
| [03](03)   | Control Structures: recursion, map, flatMap, filter, for-comprehensions                            | Calculator                                            |
| [04](04)   | Data Structures ([im]mutable, Array, List, Map, tuples)                                            | Number of small exercises                             |
| [05](05)   | Functions ([im]pure, total/partial) & Algebraic Data Types - role in functional design, using them | Poker                                                 |
| [06](06)   | SBT, single- vs multi-module projects                                                              | bulkySources sbt plugin                               |
| [08](08)   | Implicits                                                                                          | Number of small exercises                             |
| [10](10)   | HKT & Type Classes                                                                                 | Homemade JSON Encoder/Decoder & Mutable Bounded Cache |
| [11](11)   | Categories (HKT + implicits coding practice)                                                       | Categories                                            |
| [12](12)   | Error Handling - Option, Either, Try, Validated, encoding errors as ADTs                           | Payment Card Validator                                |
| [13](13)   | Processing JSON using Circe. Custom coders and decoders                                            | NBA API Parser                                        |
| [14](14)   | Unit Testing                                                                                       | Improved Calculator [02](02)                          |
| [15](15)   | Cats Core Intro. Monad Transformers                                                                | Number of small exercises                             |
| [17](17)   | Asynchronous Programming - JVM threads, Futures & Promises, synchronized, Atomic                   | Server Name Extractor                                 |
| [18](18)   | Cats Effect: IO                                                                                    | IO implementation with Declarative encoding           |
| [19](19)   | Cats Effect: Resource, ContextShift, Fiber                                                         | Min Hash Calculator                                   |
| [20](20)   | Unit Testing 2: Mocks, Compiler Testing, Parametric Reasoning                                      | Property-based tests training                         |
| [21](21)   | Shared State in FP: Ref, Deferred, Semaphore, MVar                                                 | Concurrent cache with expiration                      |
| [23](23)   | Http with http4s                                                                                   | Guessing Game                                         |
| [24](24)   | WebSocket with http4s                                                                              | Improved Guessing Game [23](23)                       |
| [26](26)   | Working with Databases from Scala: JDBC, Slick, Doobie                                             | Book Server                                           |
| [27](27)   | Akka - Actors and other Akka features                                                              | Binary Tree                                           |

## Structure

Each homework is numbered (`01`, `02` etc.). Inside the numbered folder you may find two folders: `fsharp` and `scala`. The former contains implementation of a task written with F# and the latter - with Scala.

## How to run F# (for curious Scala users)

First, you need to install .NET. That is very simple - just find your OS and lines to execute [here](https://docs.microsoft.com/en-us/dotnet/core/install/). Then you need to install [Visual Studio Code](https://code.visualstudio.com/), and Ionide plugin for it.

In case a task is written as script(s), you will see `fsx` files which you can run the following ways:

- Open folder with scripts in VS Code. Click `Ctrl + A`, and then `Alt + Enter` , which sends the selected text into REPL
- Or run `dotnet fsi scriptname.fsx` in console

Here is video example which shows both of those options: https://youtu.be/AyCYXAOFoto

If you have any questions about how to use REPL, or have problems with .net/vscode/ionide installation - let me know.

Also, you may want to read a very short F# overview. I expect it to take 30 - 60 mins at max for Scala users:

1. https://livebook.manning.com/book/concurrency-in-dot-net/appendix-a
2. https://livebook.manning.com/book/concurrency-in-dot-net/appendix-b

