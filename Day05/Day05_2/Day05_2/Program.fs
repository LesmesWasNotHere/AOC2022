//JSDHQMZGF
let parseStack (lines: string array) (stackNumber:int) =
    let pos = stackNumber * 4 + 1
    [|for line in lines do if line[pos] <> ' ' then line[pos]|]

let parseStacks lines =
    [|for i in [0..8] do parseStack lines i|]

let parseMove (line: string) =
    line[5..].Replace(" from", "").Replace(" to", "").Split(" ") |> Array.map int |> (fun x -> (x[0], x[1]-1, x[2]-1))

let parseMoves (lines: string array) =
    [|for m in lines do parseMove m|]

let doMove (stacks: char array array) (count,sourceS,toS) =
    [|for i in [0..8] do
        if (i = sourceS) then
            stacks[i][count..]
        elif (i = toS) then
            Array.append (stacks[sourceS][..count-1]) stacks[i]
        else
            stacks[i]
    |]

let rec calcMoves (remainingMoves: (int*int*int) array) (stacks: char array array) =
    if (remainingMoves.Length = 0) then
        stacks
    else
        calcMoves remainingMoves[1..] (doMove stacks remainingMoves[0])

let lines = System.IO.File.ReadAllLines("input.txt")

let resultStacks = calcMoves (parseMoves lines[10..]) (parseStacks lines[0..7])

let result = resultStacks |> Array.map (fun x -> x[0]) |> System.String.Concat

printfn "%s" result
