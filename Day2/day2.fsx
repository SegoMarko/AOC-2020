#load "..\\InputHelper\\InputHelper.fsx"
open InputHelper

let getInputValuesFromLine (line: string) = 
    let split = line.Split('-', ' ', ':')
    (int split[0], int split[1], char split[2], split[4])

let containsBetween (lower: int) (upper: int) (character: char) (str: string) =
    let count = str |> Seq.filter (fun c -> c = character) |> Seq.length
    count <= upper && count >= lower

let input = InputHelper.input __SOURCE_DIRECTORY__ 
                                               |> Seq.map (fun line -> getInputValuesFromLine line)

let result1 = input 
            |> Seq.where (fun (lower, upper, character, str) -> containsBetween lower upper character str) 
            |> Seq.length
printfn $"{result1}"

let containsOnExactlyOnePosition (pos1: int) (pos2: int) (character: char) (str: string) =
    (str[pos1 - 1] = character) <> (str[pos2 - 1] = character)

let result2 = input
            |> Seq.where (fun (pos1, pos2, character, str) -> containsOnExactlyOnePosition pos1 pos2 character str)
            |> Seq.length
printfn $"{result2}"