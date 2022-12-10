let parseInstruction (i:string) =
    if (i.Length > 4) then
        [0; int(i[5..])]
    else
        [0]

let rec getLine (input: int list) (currentIndex:int) (currentValue:int): string*int =
    if input.IsEmpty then
        ("", currentValue)
    else
        let newValue = currentValue + input.Head        
        let (line,lastValue) =  getLine input.Tail (currentIndex+1) newValue        
        ((if (currentIndex >= currentValue && currentIndex < (currentValue+3)) then "#" else ".") + line, lastValue)
        
let rec getLines (input: int list) (initialValue:int) =
    if input.IsEmpty then
        []
    else
        let (line, lastValue) = getLine input[0..39] 1 initialValue
        List.append [line] (getLines input[40..] lastValue)

let input = System.IO.File.ReadAllLines("input.txt") |> Array.toList |> List.map parseInstruction |> List.concat

for l in getLines input 1 do
    printfn "%s" l

