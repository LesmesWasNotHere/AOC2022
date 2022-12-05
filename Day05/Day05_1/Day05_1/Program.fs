//Stack numbers 0..8
let parseStack (lines: string array) (stackNumber:int) =
    let pos = stackNumber * 4 + 1
    [|for line in lines do if line[pos] <> ' ' then line[pos]|]

let parseStacks lines =
    [|for i in [0..8] do parseStack lines i|]

let parseMove (line: string) =
    line[5..].Replace(" from", "").Replace(" to", "").Split(" ") |> Array.map int |> Array.toList

let parseMoves (lines: string array) =
    [|for m in lines do parseMove m|]

let doMove (stacks: char array array) sourceS toS =
    [|for i in [0..8] do
        if (i = sourceS) then
            stacks[i][1..]
        elif (i = toS) then
            Array.append [|stacks[sourceS][0]|] stacks[i]
        else
            stacks[i]
    |]

let lines = System.IO.File.ReadAllLines("input.txt")

let mutable stacks = parseStacks lines[0..7]
let moves = parseMoves lines[10..]

for move in moves do
    let count = move[0]
    let fromS = move[1]
    let toS = move[2]
    for i in [1..count] do
        stacks <- doMove stacks (fromS-1) (toS-1) 

let result = stacks |> Array.map (fun x -> x[0]) |> System.String.Concat

printfn "%s" result
