#load "..\\InputHelper\\InputHelper.fsx"
open InputHelper

type InputLine = {First: int; Second: int; Character: char; Password: string}

let getInputValuesFromLine (line: string) = 
    let split = line.Split('-', ' ', ':')
    {First = int split[0]; Second = int split[1]; Character = char split[2]; Password = split[4]}

let containsBetween input =
    let count = input.Password |> Seq.filter (fun c -> c = input.Character) |> Seq.length
    count <= input.Second && count >= input.First

let input = InputHelper.input 2
            |> Seq.map (fun line -> getInputValuesFromLine line)

let result1 = input 
              |> Seq.where (fun line -> containsBetween line) 
              |> Seq.length
printfn $"{result1}"

let containsOnExactlyOnePosition input =
    (input.Password[input.First - 1] = input.Character) <> (input.Password[input.Second - 1] = input.Character)

let result2 = input
              |> Seq.where (fun line -> containsOnExactlyOnePosition line)
              |> Seq.length
printfn $"{result2}"