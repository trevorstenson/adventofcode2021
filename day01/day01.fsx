open System.IO

let input = File.ReadAllLines("input.txt") |> Array.map(fun x -> int x) |> Array.toList 

let rec countIncrease list =
    match list with
    | [] -> 0
    | [_] -> 0
    | x :: xs ->
        if x < xs.Head then
            1 + (countIncrease xs)
        else
            countIncrease xs

let rec countIncreaseTriplets list =
    match list with
    | [] -> 0
    | [_] -> 0
    | f::s::t::fr::xs ->
        let first = f + s + t
        let second = s + t + fr
        if first < second then
            1 + (countIncreaseTriplets (s::t::fr::xs))
        else
            (countIncreaseTriplets (s::t::fr::xs))
    | x::xs -> 0

printfn $"part1: {countIncrease input}"
printfn $"part2: {countIncreaseTriplets input}"