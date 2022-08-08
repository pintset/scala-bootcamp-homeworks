# Scala Bootcamp 2021 Homeworks

This repo contains homeworks for Scala Bootcamp 2021

![Scala CI](https://github.com/pintset/scala-bootcamp-homework/workflows/Scala%20Bootcamp%20Homeworks%20CI/badge.svg)

## Structure

Each homework is numbered (`01`, `02` etc.). Inside the numbered folder you can find two folders: `fsharp` and `scala`. The former contains implementation of a task written with F# 5 and the latter - with Scala.

## How to run F# (for curious Scala users)

First, you need to install .NET. That is very simple - just find your OS and lines to execute [here](https://docs.microsoft.com/en-us/dotnet/core/install/). Then you need to install [Visual Studio Code](https://code.visualstudio.com/), and Ionide plugin for it.

In case a task is written as script(s), you will see `fsx` files which you can run the following ways:

- Open folder with scripts in VS Code. Click `Ctrl + A`, and then `Alt + Enter` , which sends the selected text into REPL
- Or run `dotnet fsi scriptname.fsx` in console

Here is video example which shows both of those options: https://youtu.be/AyCYXAOFoto

When task is implemented as a project, then [TODO]

If you have any questions about how to use REPL, or have problems with .net/vscode/ionide installation - let me know.

Also, you may want to read a very short F# overview. I expect it to take 30 - 60 mins at max for Scala users:

1. https://livebook.manning.com/book/concurrency-in-dot-net/appendix-a
2. https://livebook.manning.com/book/concurrency-in-dot-net/appendix-b

