#load "..\\InputHelper\\InputHelper.fsx"
open InputHelper

let input = InputHelper.input 6

let getGroupAnswers (input: string seq) =
    let mutable group = System.Collections.Generic.List<string>()
    [for line in input do
        if line = ""
        then yield group |> List.ofSeq
             group <- System.Collections.Generic.List<string>()
        else group.Add line]

let groupAnswers = getGroupAnswers input
let mapAndSum mapper genericList : int =
    genericList |> List.map (fun item -> mapper item) |> List.sum

let getGroupDistinctYes (group: string list) =
    group |> String.concat "" |> Seq.distinct |> Seq.length

let result1 = 
    groupAnswers 
    |> mapAndSum getGroupDistinctYes  
printfn "%A" result1

let getGroupIntersectYes (group: string list) =
    group 
    |> List.map Set.ofSeq
    |> List.reduce (fun x y -> Set.intersect x y)
    |> Set.count

let result2 = 
    groupAnswers
    |> mapAndSum getGroupIntersectYes 
printfn "%A" result2