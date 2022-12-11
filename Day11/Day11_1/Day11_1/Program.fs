type monkey =
    new (it, op, oppar, dsquare, divby, mtrue, mfalse) = {items = it; operator = op; operatorParameter = oppar; doSquareOperator = dsquare; divisibleBy = divby; monkeyIfTrue = mtrue; monkeyIfFalse = mfalse}
    val items:int list
    val operator:char
    val operatorParameter:int
    val doSquareOperator:bool
    val divisibleBy:int
    val monkeyIfTrue:int
    val monkeyIfFalse:int
    
let parseMonkey (lines: string array) =
    let items = Array.toList (lines[1].Substring(17).Split ',') |> List.map int   
    let operator = lines[2][23]
    let doSquareOperator = lines[2][25..] = "old"
    let operatorParameter = if doSquareOperator then 0 else int (lines[2][25..])
    let divisibleBy = int (lines[3][21..])
    let monkeyIfTrue = int (lines[4][29..])
    let monkeyIfFalse = int (lines[5][30..])
    monkey(items, operator, operatorParameter, doSquareOperator, divisibleBy, monkeyIfTrue, monkeyIfFalse)

let rec parseMonkeys (lines: string array) =
    if (lines.Length = 0) then
        [||]
    else
        Array.append [|parseMonkey lines[0..6]|] (parseMonkeys lines[7..])

let monkeys = System.IO.File.ReadAllLines("input.txt") |> parseMonkeys

// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"
