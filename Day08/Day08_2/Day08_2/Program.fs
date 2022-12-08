let input = System.IO.File.ReadAllLines("input.txt") |> Array.map (fun x -> [|for c in x do int8(string(c))|])
let sizeX = input[0].Length
let sizeY = input.Length

let scoreInDirection (input: int8 array array) y x dirY dirX =
    let mutable score = 0
    let mutable targetX = x + dirX
    let mutable targetY = y + dirY
    while targetX >= 0 && targetX < input[0].Length && targetY >= 0 && targetY < input.Length do         
        if input[targetY].[targetX] <= input[y].[x] then
            score   <- score + 1
        if input[targetY].[targetX] >= input[y].[x] then
            targetX <- -1 //Exit
        else
            targetX <- targetX + dirX
            targetY <- targetY + dirY        
    score

let mutable maxScore = 0

for y in 0..sizeY-1 do
    for x in 0..sizeX-1 do
        let score = (scoreInDirection input y x 0 -1) * (scoreInDirection input y x 0 1) * (scoreInDirection input y x -1 0) * (scoreInDirection input y x 1 0)
        if (score > maxScore) then
            maxScore <- score
 
printfn "Max score %d" maxScore

        