#load @"..\InputHelper\InputHelper.fsx"
#load @"..\Day1\day1-improved.fsx"

let input = InputHelper.input 9 |> Seq.map uint64 |> Array.ofSeq

let result1 = 
    input 
    |> Array.windowed 26 
    |> Array.pick (fun window -> 
        let lastIndex = window.Length - 1
        let result = Day1Improved.getPairSum window[lastIndex] window[0 .. (lastIndex - 1)]
        match result with
        | Some _ -> None
        | None -> Some window[lastIndex])
printfn "%A" result1

let findContinousSum target (arr: uint64 array) =
    let mutable found = false
    let mutable startIndex = 0
    let mutable endIndex = 1
    let mutable result = 0UL
    while found = false do
        let current = arr[startIndex..endIndex] |> Array.sum
        if current = target
        then found <- true
             let smallest = arr[startIndex..endIndex] |> Array.min
             let largest = arr[startIndex..endIndex] |> Array.max
             result <- smallest + largest
        elif current > target
        then startIndex <- startIndex + 1
             endIndex <- startIndex + 1
        else endIndex <- endIndex + 1
    result

let result2 = input |> findContinousSum result1

printfn "%A" result2