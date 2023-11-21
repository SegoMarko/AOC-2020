#load "..\\InputHelper\\InputHelper.fsx"

let input = InputHelper.input 7

type Bag = {Name: string; mutable CanHoldShinyBag: bool option}

let mapLineToBag (line: string) : Bag * (string * int) list =
    let splittedContain = line.Split "contain" 
    let left = splittedContain[0].Split " "
    let right = splittedContain[1].Split ","
    let key = {Name = left[0] + "-" + left[1]; CanHoldShinyBag = None}
    let value = [for bagStr in right do
                    let bagStrSplitted = bagStr.Split " "
                    let childBagName = bagStrSplitted[2] + "-" + bagStrSplitted[3]
                    if childBagName <> "other-bags."
                    then yield childBagName, int bagStrSplitted[1]]
    key, value

let createDictionaryWithNames (sequence: (Bag * (string * int) list) seq) =
    let dictionary = new System.Collections.Generic.Dictionary<string, (string * int) list>()
    sequence |> Seq.iter (fun (bag, bagList) -> dictionary.Add(bag.Name, bagList))
    dictionary

let createDictionaryWithValues (sequence: (Bag * (string * int) list) seq) =
    let dictionary = new System.Collections.Generic.Dictionary<string, bool option>()
    sequence |> Seq.iter (fun (bag, _) -> dictionary.Add(bag.Name, None))
    dictionary

let containsShinyGoldDirectly bags =
    bags |> List.tryFind (fun (name, _) -> name = "shiny-gold") |> Option.isSome

let containsShinyGoldIndirectly (dictionaryWithValues: System.Collections.Generic.Dictionary<string, bool option>) bags =
    bags |> List.tryFind (fun (name, _) -> dictionaryWithValues[name].IsSome && dictionaryWithValues[name].Value) |> Option.isSome

let containsShinyGold (bags: (string * int) list) (dictionaryWithValues: System.Collections.Generic.Dictionary<string, bool option>) =
    (containsShinyGoldDirectly bags) || (containsShinyGoldIndirectly dictionaryWithValues bags)

let bags = input |> Seq.map mapLineToBag
let dictionaryWithNames = bags |> createDictionaryWithNames
let dictionaryWithValues = bags |> createDictionaryWithValues

let mutable found = true
while found do
    found <- false
    for key in dictionaryWithNames.Keys do
        if dictionaryWithValues[key].IsNone && containsShinyGold dictionaryWithNames[key] dictionaryWithValues
            then dictionaryWithValues[key] <- Some true
                 found <- true

let result1 = dictionaryWithValues |> Seq.filter (fun keyValuePair -> keyValuePair.Value.IsSome && keyValuePair.Value.Value) |> Seq.length
printfn "%A" result1

let rec countBagsInBag (bag: (string * int) list) (dictionaryWithNames: System.Collections.Generic.Dictionary<string, (string * int) list>) =
    match bag with
    | [] -> 0
    | (name, count) :: tail ->
        count
        + count * (countBagsInBag dictionaryWithNames[name] dictionaryWithNames)
        + countBagsInBag tail dictionaryWithNames

let shinyBag = dictionaryWithNames["shiny-gold"]
let result2 = countBagsInBag shinyBag dictionaryWithNames
printfn "%A" result2
