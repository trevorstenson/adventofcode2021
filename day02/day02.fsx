open System
open System.IO

type Position =
    struct
        val x: int
        val y: int
        new (a: int, b: int) = { x = a; y = b; }
    end

let input = File.ReadAllLines("input.txt") |> Array.map(fun x ->  Array.toList(x.Split ' ')) |> Array.toList

let rec calculateMultiple (inputs: list<list<string>>) (pos: Position) =
    match inputs with
    | [] -> (pos.x * pos.y)
    | x::xs ->
        let num = int x.[1]
        let new_pos =
            match x.[0] with
            | "forward" -> new Position(pos.x + num, pos.y)
            | "up" -> new Position(pos.x, pos.y - num)
            | "down" -> new Position(pos.x, pos.y + num)
            | _ -> pos
        calculateMultiple xs new_pos

let rec calculateWithAim (inputs: list<list<string>>) (pos: Position) (aim: int) =
    match inputs with
    | [] -> (pos.x * pos.y)
    | x::xs ->
        let num = int x.[1]
        let new_pos =
            match x.[0] with
            | "forward" -> new Position(pos.x + num, pos.y + (aim * num))
            | _ -> pos
        let new_aim =
            match x.[0] with
            | "up" -> aim - num
            | "down" -> aim + num
            | _ -> aim
        calculateWithAim xs new_pos new_aim

let start = new Position(0, 0)

printfn $"part1: {calculateMultiple input start}"
printfn $"part2: {calculateWithAim input start 0}"