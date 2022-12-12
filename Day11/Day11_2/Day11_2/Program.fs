//NOT WORKING

type monkey =
    new (it, op, oppar, dsquare, divby, mtrue, mfalse) = {items = it; operator = op; operatorParameter = oppar; doSquareOperator = dsquare; divisibleBy = divby; monkeyIfTrue = mtrue; monkeyIfFalse = mfalse}
    val mutable items:bigint list
    val operator:char
    val operatorParameter:bigint
    val doSquareOperator:bool
    val divisibleBy:bigint
    val monkeyIfTrue:int
    val monkeyIfFalse:int
    
let parseMonkey (lines: string array) =
    let items = Array.toList (lines[1].Substring(17).Split ',') |> List.map (fun x -> bigint(int(x)))   
    let operator = lines[2][23]
    let doSquareOperator = lines[2][25..] = "old"
    let operatorParameter = if doSquareOperator then 0I else bigint(int(lines[2][25..]))
    let divisibleBy = bigint(int(lines[3][21..]))
    let monkeyIfTrue = int (lines[4][29..])
    let monkeyIfFalse = int (lines[5][30..])
    monkey(items, operator, operatorParameter, doSquareOperator, divisibleBy, monkeyIfTrue, monkeyIfFalse)

let rec parseMonkeys (lines: string array) =
    if (lines.Length = 0) then
        [||]
    else
        Array.append [|parseMonkey lines[0..6]|] (parseMonkeys lines[7..])

let calcWorryLevelOperation (monkey:monkey) (worryLevel:bigint):bigint =
    if (monkey.doSquareOperator) then
        worryLevel*worryLevel
    else
        match monkey.operator with
            | '+' -> worryLevel + monkey.operatorParameter
            | '-' -> worryLevel - monkey.operatorParameter
            | '*' -> worryLevel * monkey.operatorParameter
            | _   -> worryLevel / monkey.operatorParameter

let inspectItem (monkeys: monkey array) (monkey:monkey) (item:bigint): bigint = 
    let worryLevelAfterUpdate = (calcWorryLevelOperation monkey item)
    let recipientMonkey = if (worryLevelAfterUpdate % monkey.divisibleBy = 0I) then monkey.monkeyIfTrue else monkey.monkeyIfFalse
    monkeys[recipientMonkey].items <- List.append monkeys[recipientMonkey].items [worryLevelAfterUpdate]
    1I

//Returns inspectioned items
let rec inspectItems (monkeys: monkey array) (monkey:monkey) (items:bigint list): bigint =
    if (items.IsEmpty) then
        0I
    else
        (inspectItem monkeys monkey items.Head) + (inspectItems monkeys monkey items.Tail)
        
//inspected items
let calculateRound (monkeys: monkey array) (monkey:monkey): bigint =
    let inspectedItems = inspectItems monkeys monkey monkey.items
    monkey.items <- []    
    inspectedItems
    
let monkeys = System.IO.File.ReadAllLines("input.txt") |> parseMonkeys

let inspectedItemsByRound = [|for i in [1..10000] do  [|for monkey in monkeys do calculateRound monkeys monkey|]|]

let totalInspectedItems = Array.reduce (fun (x:bigint array) (y: bigint array) -> [|for i in [0..(x.Length-1)] do x[i] + y[i]|]) inspectedItemsByRound

printfn "Final Round %A" totalInspectedItems

let orderedInspectedItems = totalInspectedItems  |> Array.sortDescending

printfn "Result: %A" (orderedInspectedItems[0] * orderedInspectedItems[1])
