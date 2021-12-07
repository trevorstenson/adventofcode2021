open System
open System.IO

let input = File.ReadAllLines("input.txt")

let moves = input.[0].Split(',') |> Array.toList
let boards =
    input
    |> Array.tail
    |> Array.where(fun s -> s <> "")
    |> Array.map(fun x -> x.Replace("  ", " ").Split(' ') |> Array.toList |> (fun l -> if l.Length = 6 then l.Tail else l))
    |> Seq.chunkBySize 5
    |> Seq.toList

let markBoard num board =
    board |> Array.map(fun r -> r |> List.map(fun i -> if i = num then "x" else i))

let checkBoard board =
    let bingo = ["x"; "x"; "x"; "x"; "x"]
    let rowCheck = board |> Array.exists (fun r -> r = bingo)
    let colCheck = board |> List.transpose |> List.exists (fun r -> r = bingo)
    let leftDown = [0..4] |> List.map(fun i -> (i, 4 - i))
    let rightDown = [0..4] |> List.map(fun i -> (i, i))
    let diagonalCheck =
        (leftDown |> List.map(fun t -> board.[fst t].[snd t]) |> (fun x -> x = bingo))
        || (rightDown |> List.map(fun t -> board.[fst t].[snd t]) |> (fun x -> x = bingo))
    rowCheck || colCheck || diagonalCheck

let calcScore board =
    Array.fold (fun acc r ->
                acc + List.fold (fun acc n -> if n = "x" then (acc + 0) else (acc + (int n))) 0 r
            ) 0 board

let rec solve moves boards =
    match moves with
    | [] -> -1
    | x::xs ->
        let changed = boards |> List.map(fun b -> markBoard x b)
        let solution = changed |> List.tryFind(fun b -> b |> checkBoard)
        match solution with
        | Some (board) ->
            (calcScore board) * (int x)
        | None -> solve xs changed

// let rec badSolve moves boards acc tries =
//     match moves with
//     | [] -> acc |> List.sortBy(fun b -> b) |> List.last
//     | x::xs ->
//         let changed = boards |> List.map(fun b -> markBoard x b)
//         let solution =
//             changed
//             |> List.map(fun b -> if (checkBoard b) then tries else b)
//         let new_tries = tries + 1
//         badSolve xs solution scores new_tries
        // match solution with
        // | Some (board) ->
        //     (calcScore board) * (int x)
        // | None -> solve xs changed

printfn $"{solve moves boards}"
// printfn $"bad solve: {badSolve moves boards 0}"