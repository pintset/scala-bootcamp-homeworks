#load "shouldEqualTo.fsx"
open ShouldEqualTo

// Implement scanLeft (not using scans ofc)
let scanLeft folder state =
    List.fold (fun xs x -> folder xs.Head x :: xs) [ state ] >> List.rev

let numbers = [ 1..100 ]
numbers |> List.scan (+) 0 |> shouldEqualTo (numbers |> scanLeft (+) 0)

// https://twitter.com/allenholub/status/1357115515672555520/photo/1
// pass the interview
let count xs =
    let folder state x =
        match state with
        | [] -> [ x, 1 ]
        | (h, c) :: t when h = x -> (h, c + 1) :: t
        | xs ->  (x, 1) :: xs
    xs |> Seq.fold folder [] |> List.rev

"aaaabbbcca" |> count |> shouldEqualTo [ ('a', 4); ('b', 3); ('c', 2); ('a', 1) ]
[ 1; 1; 1; 1; 2; 2; 2; 3; 3; 1 ] |> count |> shouldEqualTo [ (1, 4); (2, 3); (3, 2); (1, 1) ]