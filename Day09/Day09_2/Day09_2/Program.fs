let ropeLength = 10

let calcStep distance : int =
        if (distance <> 0) then distance / (abs distance) else 0

let moveHead (move: char) ((posHeadY, posHeadX):(int*int)): (int*int) =
    match move with
        | 'U' -> (posHeadY + 1, posHeadX)
        | 'D' -> (posHeadY - 1, posHeadX)
        | 'L' -> (posHeadY, posHeadX - 1)
        |  _  -> (posHeadY, posHeadX + 1)

let rec moveMiddleKnots ((headPositionY, headPositionX): (int*int)) (knots: (int*int) list): (int*int) list =
    if (knots.IsEmpty) then
        []
    else
        let (tailPositionY, tailPositionX) = knots.Head
        let (tailDistanceY, tailDistanceX) = (headPositionY - tailPositionY, headPositionX - tailPositionX)
        let newTailPos = if (max (abs tailDistanceY) (abs tailDistanceX)) > 1 then (tailPositionY + (calcStep tailDistanceY), tailPositionX + (calcStep tailDistanceX)) else (tailPositionY, tailPositionX)
        List.append [newTailPos] (moveMiddleKnots newTailPos knots.Tail)

let rec applyMoves (input: char list) (partialResults: (int*int) list list): (int*int) list list =
    if input.IsEmpty then
        partialResults
    else            
        let newHeadPos = moveHead input.Head partialResults.Head.Head
        let headPosList = [newHeadPos]
        let middleKnots = (moveMiddleKnots newHeadPos partialResults.Head[1..ropeLength-1])
        applyMoves input.Tail (List.append [List.append headPosList middleKnots] partialResults)

let input = System.IO.File.ReadAllLines("input.txt") |> Array.toList |> List.collect (fun x -> List.init (int x[2..]) (fun y -> x[0])) 
let positions = applyMoves input [List.init ropeLength (fun x -> (0,0))] |> List.rev
let tailPositions = positions |> List.filter (fun x -> x.Length = ropeLength) |> List.map (fun x -> x.[ropeLength-1])
let differentTailPositions = (tailPositions |> List.countBy id)

printfn "Different tail positions %d" differentTailPositions.Length
