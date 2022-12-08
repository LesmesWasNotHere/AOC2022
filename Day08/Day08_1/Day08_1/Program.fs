let input = System.IO.File.ReadAllLines("input.txt") |> Array.map (fun x -> [|for c in x do int8(string(c))|])
let sizeX = input[0].Length
let sizeY = input.Length

let tallestAround (input: int8 array array) (state: int8 array2d) y x dirY dirX =
    let targetX = x + dirX
    let targetY = y + dirY
    if targetX < 0 || targetX >= state.GetLength(1) || targetY < 0 || targetY >= state.GetLength(0) then -1y else Array.max [| (input[targetY].[targetX]); state[targetY,targetX] |]

let tallestTreeEast  = Array2D.zeroCreate<int8> sizeY sizeX
let tallestTreeNorth = Array2D.zeroCreate<int8> sizeY sizeX
let tallestTreeWest  = Array2D.zeroCreate<int8> sizeY sizeX
let tallestTreeSouth = Array2D.zeroCreate<int8> sizeY sizeX

for y in 0..sizeY-1 do
    for x in 0..sizeX-1 do
        Array2D.set<int8> tallestTreeWest y x (tallestAround input tallestTreeWest y x 0 -1)
        Array2D.set<int8> tallestTreeNorth y x (tallestAround input tallestTreeNorth y x -1 0)

for y in sizeY-1..-1..0 do
    for x in sizeX-1..-1..0 do
        Array2D.set<int8> tallestTreeEast y x (tallestAround input tallestTreeEast y x 0 1)
        Array2D.set<int8> tallestTreeSouth y x (tallestAround input tallestTreeSouth y x 1 0)
 
let mutable visibleTrees = 0

for y in 0..sizeY-1 do
    for x in 0..sizeX-1 do
        if tallestTreeEast[y,x] < input[y].[x] 
            || tallestTreeNorth[y,x] < input[y].[x] 
            || tallestTreeWest[y,x] < input[y].[x] 
            || tallestTreeSouth[y,x] < input[y].[x] then
            visibleTrees <- visibleTrees + 1

printfn "Visible trees %d" visibleTrees

        