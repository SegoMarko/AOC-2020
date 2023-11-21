#load @"..\InputHelper\InputHelper.fsx"

(* 
    Thankfully in input given to me there was no adapters that had jolt-distance 2 from any other adapter.
    That would complicate things a little more since it would require more complex grouping of adapters.

    General solution is to find 'adapter groups' which are series of adapters in a row with a jolt-distance 1.
    We can split them apart with adapters that are distanced 3 from previous adapter (meaning starting a new group).
    Since every one of those groups must exist (irrelevant of which adapters in a group are chosen) then it only
    remains to calculate combinations of adapters in every group and multiply them together.
    Combinations inside groups are found with probability theory selecting discrete items from a group
    e.g. 2 of 2 (2 over 2) + 1 of 2 (2 over 1) + 0 of 2 (2 over 0) = 4 combinations
    Also every group of adapters must include both far left (smallest) and far right (largest) adapter since
    the next adapter (in the next group) is distanced 3, because of this they are excluded from calculating
    combinations.

    If group contains more than 5 adapters that means we have more complex case of calculating combinations
    inside a single group because we still need to follow the 3 jolt-distance rule inside single group when
    calculating combinations. But in my input there were no such cases. Anyhow i implemented that aswell.
    Because of this problem 'calculateAdapterGroupCombinations' is more complex than neccessary.
*)

let temp = InputHelper.input 10 |> Seq.map int |> List.ofSeq
let input = temp @ [0] @ [(temp |> List.max |> (+) 3)]

let joltDifferences =
    input
    |> List.sort
    |> List.pairwise
    |> List.map (fun (first, second) -> second - first)

let oneJoltDifferences = joltDifferences |> List.where (fun difference -> difference = 1) |> List.length
let threeJoltDifferences = joltDifferences |> List.where (fun difference -> difference = 3) |> List.length
let result1 = oneJoltDifferences * threeJoltDifferences
printfn "%A" result1

let indexesToSplitBy = joltDifferences |> List.mapi (fun i diff -> if diff = 3 then Some i else None) |> List.choose id
let getAdapterGroups indexesForSplit (differences: int list) =
    let mutable lastIndex = 0
    [for index in indexesForSplit do
        yield differences[lastIndex .. index]
        lastIndex <- index + 1]

let groupedWithDiff1 = 
    joltDifferences 
    |> getAdapterGroups indexesToSplitBy 
    |> List.where (fun l -> (List.isEmpty l) = false)

let groupedWithDiff1AndPossibleChoose =
    groupedWithDiff1
    |> List.where (fun l -> l.Length > 2)

let factorial num = 
    match num with
    | 0 -> 1
    | 1 -> 1
    | _ -> [1..num] |> List.reduce (*)

let firstOverSecond first second =
    (factorial first) / ((factorial second) * factorial (first - second))

let calculateAdapterGroupCombinations (adapterGroup: int list) : int =
    match adapterGroup.Length with
    | length when length > 2 ->
        let adaptersToChoose = adapterGroup.Length - 2

        let miniGroup = adaptersToChoose / 3
        let restOfMiniGroup = adaptersToChoose - (miniGroup * 3)
        let mutable combinations = 1

        if miniGroup > 0 then
            for _ in [1 .. miniGroup] do
                let mutable temp = 0
                for i in [1 .. 3] do
                    temp <- temp + (firstOverSecond 3 i)

                combinations <- combinations * temp
                combinations <- combinations - restOfMiniGroup

        if restOfMiniGroup > 0 then
            let mutable temp = 0
            for i in [0 .. restOfMiniGroup] do
                temp <- temp + (firstOverSecond restOfMiniGroup i)
            combinations <- combinations * temp

        combinations
    | _ -> 1

let result2 = 
    groupedWithDiff1AndPossibleChoose 
    |> List.map (fun group -> calculateAdapterGroupCombinations group) 
    |> List.map uint64 
    |> List.reduce (*)
printfn "%A" result2