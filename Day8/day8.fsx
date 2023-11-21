#load "..\\InputHelper\\InputHelper.fsx"

let input = InputHelper.input 8

type Instruction = {Name: string; Parameter: int}
type InstructionResult = {Accumulator: int; NextInstructionIndex: int}

let inputToInstructions (lines: string seq) : Instruction array =
    [|for line in lines do
        let splitted = line.Split " "
        yield {Name = splitted[0]; Parameter = int splitted[1]}|]

let executeInstruction (instruction: Instruction) instructionIndex accumulator : InstructionResult =
    match instruction.Name with
    | "nop" -> {Accumulator = accumulator;                         NextInstructionIndex = instructionIndex + 1}
    | "acc" -> {Accumulator = accumulator + instruction.Parameter; NextInstructionIndex = instructionIndex + 1}
    | "jmp" -> {Accumulator = accumulator;                         NextInstructionIndex = instructionIndex + instruction.Parameter}
    | _ -> failwith "data error"

let executeInstructions (instructions: Instruction array) =
    let mutable accumulator = 0
    let mutable index = 0
    let set = new System.Collections.Generic.HashSet<int>()
    let mutable shouldContinue = true
    let mutable success = false
    while shouldContinue do
        if set.Contains(index)
        then shouldContinue <- false
        elif index >= instructions.Length
        then shouldContinue <- false
             success <- true
        else set.Add(index) |> ignore
             let instruction = instructions[index]
             let result = executeInstruction instruction index accumulator
             accumulator <- result.Accumulator
             index <- result.NextInstructionIndex
    accumulator, success

let fixInstruction instruction =
    match instruction.Name with
    | "jmp" -> {Name = "nop"; Parameter = instruction.Parameter}
    | "nop" -> {Name = "jmp"; Parameter = instruction.Parameter}
    | _ -> failwith "nope"

let fixAndExecuteInstructions (instructions: Instruction array) =
    let mutable success = false
    let mutable accumulator = 0
    let mutable lastChanged = -1
    while success = false do
        if lastChanged <> -1
        then instructions[lastChanged] <- fixInstruction instructions[lastChanged]

        let (index, instruction) = instructions 
                                   |> Array.indexed 
                                   |> Array.find (fun (i, element) -> 
                                        (element.Name = "jmp" || element.Name = "nop") && i > lastChanged)
        instructions[index] <- fixInstruction instruction

        lastChanged <- index

        let (acc, succ) = executeInstructions instructions
        success <- succ
        accumulator <- acc

    accumulator

let instructions = input |> inputToInstructions
let result1 = instructions |> executeInstructions
printfn "%A" result1

let result2 = instructions |> fixAndExecuteInstructions
printfn "%A" result2