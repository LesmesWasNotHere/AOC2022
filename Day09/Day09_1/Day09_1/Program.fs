
let calcStep distance : int =
        if (distance <> 0) then distance / (abs distance) else 0

let rec applyMoves (input: char list) ((posHeadY, posHeadX):int*int) ((posTailY, posTailX):int*int) partialResults =
    if input.IsEmpty then
        partialResults
    else
        let (newHeadPositionY, newHeadPositionX) = match input.Head with
                                                    | 'U' -> (posHeadY + 1, posHeadX)
                                                    | 'D' -> (posHeadY - 1, posHeadX)
                                                    | 'L' -> (posHeadY, posHeadX - 1)
                                                    |  _  -> (posHeadY, posHeadX + 1)
        let (tailDistanceY, tailDistanceX) = (newHeadPositionY - posTailY, newHeadPositionX - posTailX)
        let (newTailPosY, newTailPosX) = if (max (abs tailDistanceY) (abs tailDistanceX)) > 1 then (posTailY + (calcStep tailDistanceY), posTailX + (calcStep tailDistanceX)) else (posTailY, posTailX)
        applyMoves input.Tail (newHeadPositionY, newHeadPositionX) (newTailPosY, newTailPosX) (List.append [(newTailPosY, newTailPosX)] partialResults)

let input = System.IO.File.ReadAllLines("input.txt") |> Array.toList |> List.collect (fun x -> List.init (int x[2..]) (fun y -> x[0])) 
let tailPositions = List.append [(0,0)] (applyMoves input (0,0) (0,0) [] |> List.rev)

printfn "Different tail positions: %d" (tailPositions |> List.countBy id).Length