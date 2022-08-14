#r "nuget: Http.fs"
#r "nuget: Thoth.Json.Net"

open System

open HttpFs.Client
open Hopac
open Hopac.Infixes

module Request =
    open Thoth.Json.Net

    let private caseStrategy = CamelCase

    let createUrl method url =
        "http://localhost:9001" + url
        |> Request.createUrl method

    let responseAs<'a> request =
        request
        |>  Request.responseAsString
        >>- fun s -> Decode.Auto.fromString<'a>(s, caseStrategy)
        >>- function Ok x -> x | Error e -> failwith e

    let body o =
        Encode.Auto.toString (0, o, caseStrategy) |> Request.bodyString 

type Repo<'a>(path) =
    let getAll =
        Request.createUrl Get path
        |> Request.responseAs<'a array>

    let getById (id: Guid) =
        Request.createUrl Get $"{path}/{id}"
        |> Request.responseAs<'a>

    let create o =
        Request.createUrl Post path
        |> Request.body o
        |> Request.responseAs<'a>

    let update o =
        Request.createUrl Put path
        |> Request.body o
        |> Request.responseAs<'a>

    let delete (id: Guid) =
        Request.createUrl Delete $"{path}/{id}"
        |> Request.responseAsString
        |> Job.Ignore

    member _.GetAll   = getAll
    member _.GetById  = getById
    member _.Create o = create o
    member _.Update o = update o
    member _.Delete   = delete

type Author = { Id: Guid; Name: string; Birthday: DateTimeOffset }

let authorJob = job {
    let repo = Repo<Author>("/authors")

    let! items = repo.GetAll
    let! item = items.[0].Id |> repo.GetById
    assert (item = items.[0])

    let newItemName = "New"
    let newBirthDay = "1995-06-01"
    let! newItem = repo.Create {| Name = newItemName; Birthday = newBirthDay |}
    assert (newItemName = newItem.Name)

    let updatedItemName = "Updated"
    let updatedBirthday = "1990-06-01"
    let! updatedItem = repo.Update {| Id = newItem.Id; Name = updatedItemName; Birthday = updatedBirthday |}
    assert (updatedItem.Id = newItem.Id)
    assert (updatedItem.Name = updatedItemName)
    assert (updatedItem.Birthday = DateTimeOffset.Parse(updatedBirthday))

    let! _ = repo.Delete updatedItem.Id
    let! items = repo.GetAll
    let exists = items |> Array.exists(fun i -> i.Id = updatedItem.Id)
    assert (not exists)
}

type Genre = { Id: Guid; Name: string }

let genreJob = job {
    let repo = Repo<Genre>("/genres")

    let! items = repo.GetAll
    let! item = items.[0].Id |> repo.GetById
    assert (item = items.[0])

    let newItemName = "New"
    let! newItem = repo.Create {| Name = newItemName |}
    assert (newItemName = newItem.Name)

    let updatedItemName = "Updated"
    let! updatedItem = repo.Update { newItem with Name = updatedItemName }
    assert (updatedItem.Id = newItem.Id)
    assert (updatedItem.Name = updatedItemName)

    let! _ = repo.Delete updatedItem.Id
    let! items = repo.GetAll
    let exists = items |> Array.exists(fun i -> i.Id = updatedItem.Id)
    assert (not exists)
}

type Book =
    { Id: Guid
      Title: string
      Year: String
      AuthorId: Guid
      GenreId: Guid }

let bookJob = job {
    let path = "/books"

    let repoFull = Repo<{| Id: Guid; Title: string; Year: String; Author: Author; Genre: Genre |}>(path)
    let repo = Repo<Book>(path)

    let! items = repoFull.GetAll
    let fullItem = items.[0]
    let! item = fullItem.Id |> repo.GetById
    assert ({ Id = fullItem.Id; Title = fullItem.Title; Year = fullItem.Year; AuthorId = fullItem.Author.Id; GenreId = fullItem.Genre.Id } = item)

    let newItemName = "New"
    let! newItem = repo.Create {| Title = newItemName; Year = "2021"; AuthorId = item.AuthorId; GenreId = item.GenreId |}
    assert (newItemName = newItem.Title)

    let updatedItemName = "Updated"
    let! updatedItem = repo.Update { newItem with Title = updatedItemName }
    assert (updatedItem.Id = newItem.Id)
    assert (updatedItem.Title = updatedItemName)

    let! _ = repo.Delete updatedItem.Id
    let! items = repoFull.GetAll
    let exists = items |> Array.exists(fun i -> i.Id = updatedItem.Id)
    assert (not exists)
}

run authorJob
run genreJob
run bookJob
