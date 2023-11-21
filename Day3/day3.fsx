#load "..\\InputHelper\\InputHelper.fsx"

let input = InputHelper.input 3 |> Array.ofSeq

let getArrayValue (array: string array) x y =
    let normalizedX = x % array[0].Length
    let normalizedY = y % array.Length
    array[normalizedY][normalizedX]


let getXRightAndYDownCount deltaX deltaY (array: string array)  = 
    let mutable count = 0
    let mutable currentX = 0
    let mutable currentY = 0
    while currentY <= array.Length do
        if getArrayValue array currentX currentY = '#'
        then count <- count + 1
        currentX <- currentX + deltaX 
        currentY <- currentY + deltaY 
    count

let result1 = input |> getXRightAndYDownCount 3 1
printfn $"{result1}"

let result2 = [| 
    getXRightAndYDownCount 1 1 input
    getXRightAndYDownCount 3 1 input
    getXRightAndYDownCount 5 1 input
    getXRightAndYDownCount 7 1 input
    getXRightAndYDownCount 1 2 input
              |] |> Array.reduce (*)
printfn $"{result2}"