#load "..\\InputHelper\\InputHelper.fsx"
open InputHelper

let getPairSum (target: int) (number: int) (sequence: seq<int>) =
    let found = sequence |> Seq.tryFind (fun x -> x = target - number)
    match found with
    | Some f -> Some (number, f)
    | None -> None

let rec getTripletSum (target: int) (number: int) (sequence: seq<int>)  =
    let newTarget = target - number
    let pairSum = sequence |> Seq.tryPick (fun newNumber -> getPairSum newTarget newNumber sequence)
    match pairSum with
    | Some (second, third) -> Some (number, second, third)
    | None -> None

let input = InputHelper.input 1 |> Seq.map int
let result1 = input |> Seq.tryPick (fun number -> getPairSum 2020 number input)
match result1 with
| Some (a, b) -> printfn $"{a * b}"
| None -> failwith "input error"

let result2 = input |> Seq.tryPick (fun number -> getTripletSum 2020 number input)
match result2 with
| Some (a, b, c) -> printfn $"{a * b * c}"
| None -> failwith "input error"