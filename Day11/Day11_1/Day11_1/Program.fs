//There should be a better solution without mutable lists

type monkey =
    new (it, op, oppar, dsquare, divby, mtrue, mfalse) = {items = it; operator = op; operatorParameter = oppar; doSquareOperator = dsquare; divisibleBy = divby; monkeyIfTrue = mtrue; monkeyIfFalse = mfalse}
    val mutable items:int list
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

let calcWorryLevelOperation (monkey:monkey) (worryLevel:int):int =
    if (monkey.doSquareOperator) then
        worryLevel*worryLevel
    else
        match monkey.operator with
            | '+' -> worryLevel + monkey.operatorParameter
            | '-' -> worryLevel - monkey.operatorParameter
            | '*' -> worryLevel * monkey.operatorParameter
            | _   -> worryLevel / monkey.operatorParameter

let inspectItem (monkeys: monkey array) (monkey:monkey) (item:int): int = 
    let worryLevelAfterUpdate: int = (calcWorryLevelOperation monkey item) / 3
    let recipientMonkey = if (worryLevelAfterUpdate % monkey.divisibleBy = 0) then monkey.monkeyIfTrue else monkey.monkeyIfFalse
    monkeys[recipientMonkey].items <- List.append monkeys[recipientMonkey].items [worryLevelAfterUpdate]
    1

//Returns inspectioned items
let rec inspectItems (monkeys: monkey array) (monkey:monkey) (items:int list): int =
    if (items.IsEmpty) then
        0
    else
        (inspectItem monkeys monkey items.Head) + (inspectItems monkeys monkey items.Tail)
        
//inspected items
let calculateRound (monkeys: monkey array) (monkey:monkey): int =
    let inspectedItems = inspectItems monkeys monkey monkey.items
    monkey.items <- []    
    inspectedItems
    
let monkeys = System.IO.File.ReadAllLines("input.txt") |> parseMonkeys

let inspectedItemsByRound = [|for i in [1..20] do  [|for monkey in monkeys do calculateRound monkeys monkey|]|]

let orderedInspectedItems = Array.reduce (fun (x:int array) (y: int array) -> [|for i in [0..(x.Length-1)] do x[i] + y[i]|]) inspectedItemsByRound |> Array.sortDescending

printfn "Result: %d" (orderedInspectedItems[0] * orderedInspectedItems[1])
