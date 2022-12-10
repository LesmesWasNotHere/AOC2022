let parseInstruction (i:string) =
    if (i.Length > 4) then
        [0; int(i[5..])]
    else
        [0]

let rec getSignalStrength (input: int list) (currentIndex:int) (currentValue:int) (indexesToPick:int list): int list =
    if input.IsEmpty || indexesToPick.IsEmpty then
        []
    else
        let newValue = currentValue + input.Head
        if (currentIndex = indexesToPick.Head) then
            List.append [currentIndex*currentValue] (getSignalStrength input.Tail (currentIndex+1) newValue indexesToPick.Tail)
        else
            getSignalStrength input.Tail (currentIndex+1) newValue indexesToPick

let input = System.IO.File.ReadAllLines("input.txt") |> Array.toList |> List.map parseInstruction |> List.concat

printfn "Result: %d" (getSignalStrength input 1 1 [20..40..220] |> List.sum)