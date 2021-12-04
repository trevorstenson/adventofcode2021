open System
open System.IO
open System.Collections.Generic

let input = File.ReadAllLines("input.txt") |> Array.map(fun x -> Seq.toList x)
let index_list = [0..(input.[0].Length - 1)]

let ofSeq (src:seq<'a * 'b>) = 
    let d = new Dictionary<'a, 'b>()
    for (k,v) in src do
        d.Add(k,v)
    d

let all_idx_digits = index_list |> List.map(fun i -> input |> Array.map(fun b -> b.[i]))

let gammaFunc zero one = if zero > one then "0" else "1"
let epsilonFunc zero one = if zero < one then "0" else "1"
let calc callback =
    all_idx_digits
    |> List.map(fun list ->
        list
        |> Seq.countBy (fun n -> n) 
        |> ofSeq
        |> (fun dict ->
            let zero_count = dict.['0']
            let one_count = dict.['1']
            callback zero_count one_count)
    ) |> String.Concat

printfn $"gamma {calc gammaFunc}"
printfn $"epsilon {calc epsilonFunc}"
// 2987 * 1108 == 3309596