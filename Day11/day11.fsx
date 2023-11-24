#load @"..\InputHelper\InputHelper.fsx"

let input = InputHelper.input 11

type SeatState = 
    | Floor
    | Empty
    | Occupied

let printBoard (board: SeatState array array) = 
    for row in board do
        printfn ""
        for seat in row do
            match seat with
            | Floor -> printf "."
            | Empty -> printf "L"
            | Occupied -> printf "#"

let normalizeY y (board: SeatState array array) =
    if y >= 0 && y < board.Length then Some y
    else None

let normalizeX x (board: SeatState array array) =
    if x >= 0 && x < board[0].Length then Some x
    else None

let getState y x (board: SeatState array array) =
    let normalizedY = normalizeY y board
    let normalizedX = normalizeX x board
    match normalizedX, normalizedY with
    | Some(nx), Some(ny) -> Some(board[ny][nx])
    | _ -> None

let getNumOfOccupiedNeighbours y x (board: SeatState array array) = 
    [getState (y-1) (x-1) board;
    getState (y-1) (x) board;
    getState (y-1) (x+1) board;
    getState (y) (x-1) board;
    getState (y) (x+1) board;
    getState (y+1) (x-1) board;
    getState (y+1) (x) board;
    getState (y+1) (x+1) board]
    |> List.choose id
    |> List.where (fun state -> state = Occupied)
    |> List.length

let customZip (l1: 'a list) (l2: 'b list) =
    let ending = if l1.Length > l2.Length then l2.Length else l1.Length
    [for index in [0..ending-1] do
        yield (l1[index], l2[index])]

let getNumOfOccupiedNeighbours2 y x (board: SeatState array array) = 
    let notFloor = (fun (s: SeatState) -> s <> Floor)
    [
        [for row in [y-1..-1..0] do
            yield board[row][x]]
        |> List.tryFind notFloor
        [for row in [y+1..board.Length-1] do
            yield board[row][x]]
        |> List.tryFind notFloor
        [for column in [x-1..-1..0] do
            yield board[y][column]]
        |> List.tryFind notFloor
        [for column in [x+1..board[0].Length-1] do
            yield board[y][column]]
        |> List.tryFind notFloor
        [for (row, column) in customZip [y-1..-1..0] [x-1..-1..0] do
            yield board[row][column]]
        |> List.tryFind notFloor
        [for (row, column) in customZip [y-1..-1..0] [x+1..board[0].Length-1] do
            yield board[row][column]]
        |> List.tryFind notFloor
        [for (row, column) in customZip [y+1..board.Length-1] [x-1..-1..0] do
            yield board[row][column]]
        |> List.tryFind notFloor
        [for (row, column) in customZip [y+1..board.Length-1] [x+1..board[0].Length-1] do
            yield board[row][column]]
        |> List.tryFind notFloor
    ]
    |> List.choose id
    |> List.where (fun state -> state = Occupied)
    |> List.length

let hasOccupiedNeighbour y x board = (getNumOfOccupiedNeighbours y x board) >= 1
let has4OrMoreOccupiedNeighbours y x board = (getNumOfOccupiedNeighbours y x board) >= 4

let hasOccupiedNeighbour2 y x board = (getNumOfOccupiedNeighbours2 y x board) >= 1
let has5OrMoreOccupiedNeighbours2 y x board = (getNumOfOccupiedNeighbours2 y x board) >= 5

let getBoardAfterSimulation getNextStateFunc board =
    let mutable nextBoard =  getNextStateFunc board
    let mutable temp = board
    while temp <> nextBoard do 
        temp <- nextBoard
        nextBoard <- getNextStateFunc temp
    nextBoard

let getNextState emptyToOccupiedTransition occupiedToEmptyTransition (board: SeatState array array) : SeatState array array=
    [|for y, row in board |> Array.indexed do
        yield [|for x, seat in row |> Array.indexed do
                  yield match seat with
                        | Floor -> Floor
                        | Empty -> if emptyToOccupiedTransition y x board then Empty else Occupied
                        | Occupied -> if occupiedToEmptyTransition y x board then Empty else Occupied|]|]
                
let getStartingBoard =
    [|for row in input do
      yield [|for seat in row do
              yield match seat with
                    | 'L' -> Empty
                    | '.' -> Floor
                    | _ -> failwith "input error"|]|]

let startingBoard = getStartingBoard
let getNextState1 = getNextState hasOccupiedNeighbour has4OrMoreOccupiedNeighbours
let endBoard1 = getBoardAfterSimulation getNextState1 startingBoard
let result1 = [for row in endBoard1 do for seat in row do if seat = Occupied then 1 else 0] |> List.sum
printfn "%A" result1

let getNextState2 = getNextState hasOccupiedNeighbour2 has5OrMoreOccupiedNeighbours2
let endBoard2 = getBoardAfterSimulation getNextState2 startingBoard
let result2 = [for row in endBoard2 do for seat in row do if seat = Occupied then 1 else 0] |> List.sum
printfn "%A" result2