#load @"..\InputHelper\InputHelper.fsx"

let input = InputHelper.input 12

type PositionDelta = {North: int; South: int; West: int; East: int}
type TurnDirection = Left | Right
type MoveDirection = North | South | West | East
type Ship = {PositionDelta: PositionDelta; Direction: MoveDirection}

type Degrees = Degree of int
type Distance = Distance of int

type Command = 
    | Turn of TurnDirection * Degrees
    | Move of MoveDirection * Distance
    | MoveForward of Distance

let getCommands (lines: string seq) = 
    lines |> Seq.map (fun line ->
        let first = line[0]
        let second = int line[1..]
        match first with
        | 'N' -> Move (North, Distance second)
        | 'S' -> Move (South, Distance second)
        | 'W' -> Move (West, Distance second)
        | 'E' -> Move (East, Distance second)
        | 'L' -> Turn (Left, Degree second)
        | 'R' -> Turn (Right, Degree second)
        | 'F' -> MoveForward (Distance second)
        | _ -> failwith "input error")

let turnShip (turn: (TurnDirection * Degrees)) (ship: Ship) : MoveDirection =
    match turn with
        | _, Degree 180 -> 
            match ship.Direction with
            | North -> South
            | South -> North
            | West -> East
            | East -> West
        | turnDirection, Degree 90 ->
            match turnDirection with
                | Left ->
                    match ship.Direction with
                    | North -> West
                    | South -> East
                    | West -> South
                    | East -> North
                | Right ->
                    match ship.Direction with
                    | North -> East
                    | South -> West
                    | West -> North
                    | East -> South
        | turnDirection, Degree 270 ->
            match turnDirection with
                | Left ->
                    match ship.Direction with
                    | North -> East
                    | South -> West
                    | West -> North
                    | East -> South
                | Right ->
                    match ship.Direction with
                    | North -> West
                    | South -> East
                    | West -> South
                    | East -> North
        | t, d -> failwith $"turn input error {t} {d}"

let moveShip (distance: (MoveDirection * Distance)) (ship: Ship) : PositionDelta =
    match distance with
    | North, Distance distance -> {ship.PositionDelta with North = ship.PositionDelta.North + distance}
    | South, Distance distance -> {ship.PositionDelta with South = ship.PositionDelta.South + distance}
    | West, Distance distance -> {ship.PositionDelta with West = ship.PositionDelta.West + distance}
    | East, Distance distance -> {ship.PositionDelta with East = ship.PositionDelta.East + distance}

let moveShipForward (distance: Distance) (ship: Ship) : PositionDelta = moveShip (ship.Direction, distance) ship

let executeCommand (ship: Ship) (command: Command) : Ship =
    match command with
    | Turn (turnDirection, degrees) ->
        {PositionDelta = ship.PositionDelta; Direction = turnShip (turnDirection, degrees) ship}
    | Move (moveDirection, distance) ->
        {PositionDelta = moveShip (moveDirection, distance) ship ; Direction = ship.Direction}
    | MoveForward (distance) ->
        {PositionDelta = moveShipForward distance ship ; Direction = ship.Direction}

let getManhattanDistance (positionDelta: PositionDelta) : int =
    abs (positionDelta.North - positionDelta.South) + abs (positionDelta.West - positionDelta.East)

let commands =  input |> getCommands |> List.ofSeq
let startingShip = {PositionDelta = {North = 0; South = 0; West = 0; East = 0}; Direction = East}

let endingShip = commands |> List.fold (fun currentShip command -> executeCommand currentShip command) startingShip 
let result1 = getManhattanDistance endingShip.PositionDelta
printfn "%A" result1