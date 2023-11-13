#load "..\\InputHelper\\InputHelper.fsx"
open InputHelper

let getPairSum (sequence: seq<int>) =
    let set = System.Collections.Generic.HashSet()
    let result = sequence |> Seq.tryPick (fun current ->
            let target = 2020 - current
            if set.Contains target
                then Some (current, target)
                else set.Add current |> ignore
                     None)
    result

let getSeqIndexedValues sequence =
    sequence |> Seq.mapi (fun i s -> (s, i))

let rec getTripletSum (sequence: seq<int>)  =
    let dictionary = System.Collections.Generic.Dictionary<int, int * int>()
    for (first, i) in sequence |> getSeqIndexedValues do
        for (second, j) in sequence |> Seq.skip (i+1) |> getSeqIndexedValues do
            if dictionary.ContainsKey(first + second) = false
            then dictionary.Add(first + second, (first, second))

    let result = sequence |> Seq.tryPick (fun current -> 
            let target = 2020 - current
            if dictionary.ContainsKey target
            then let (second, third) = dictionary[target]
                 Some (current, second, third)
            else None)
    result

let input = InputHelper.input 1 |> Seq.map int
let result1 = getPairSum input
match result1 with
| Some (a, b) -> printfn $"{a * b}"
| None -> failwith "input error"

let result2 = getTripletSum input
match result2 with
| Some (a, b, c) -> printfn $"{a * b * c}"
| None -> failwith "input error"
