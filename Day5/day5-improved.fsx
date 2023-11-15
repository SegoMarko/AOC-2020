#load "..\\InputHelper\\InputHelper.fsx"
open InputHelper

let input = InputHelper.input 5

type Direction = Upper | Lower
type Range = Range of (int * int) | Single of int

let getUpper (range: (int * int)) = 
    let (lower, upper) = range
    let lowerBoundary = (lower + upper) / 2
    if lowerBoundary + 1 = upper
    then Single (upper)
    else Range (lowerBoundary + 1, upper)
let getLower (range: (int * int)) = 
    let (lower, upper) = range
    let lowerBoundary = (lower + upper) / 2
    if lower = lowerBoundary
    then Single (lower)
    else Range (lower, lowerBoundary)

let rec getSeatValue (input: Direction list) (start: Range) : int =
    match start with
    | Single point -> point
    | Range (lower, upper) ->
        match input with
        | head :: tail ->
            match head with
            | Upper -> getSeatValue tail (getUpper (lower, upper))
            | Lower -> getSeatValue tail (getLower (lower, upper))
        | [] -> failwith "data error"

let getSeatId (input: Direction list) : int =
    let rowId = getSeatValue (input |> List.take 7) (Range (0, 127))
    let columnId = getSeatValue (input |> List.skip 7) (Range (0, 7))
    rowId * 8 + columnId

let mapInputLine (line: string) : Direction list =
    [for c in line do
        match c with
        | 'F' -> yield Lower
        | 'B' -> yield Upper
        | 'L' -> yield Lower
        | 'R' -> yield Upper
        | _ -> failwith "input error"]

let result1 = 
    input 
    |> Seq.map (fun line -> line |> mapInputLine |> getSeatId) 
    |> Seq.max
printfn "%A" result1

let getRemainingPositions (positions: int Set) =
    [for column in 0..7 do
        for row in 0..127 do
            let position = row * 8 + column
            if (positions |> Set.contains position) = false
            then yield position]

let filterPositionNotOnTheEdge (seatId: int) =
    seatId / 8 <> 0 && seatId / 8 <> 127

let getSolution (seatIds: int list) : int =
    let distanceMoreThan1 (index: int) (current: int) : bool =
        if index = -1 || index = seatIds.Length
        then true
        else System.Math.Abs (seatIds[index] - current) > 1
    let mutable result = -1
    let index = 0
    while result = -1 do
        let current = seatIds[index]
        if distanceMoreThan1 (index - 1) current && distanceMoreThan1 (index + 1) current
        then result <- current
    result

let result2 = 
    input 
    |> Seq.map (fun line -> line |> mapInputLine |> getSeatId)
    |> set 
    |> getRemainingPositions
    |> List.filter filterPositionNotOnTheEdge
    |> List.sort
    |> getSolution
printfn "%A" result2