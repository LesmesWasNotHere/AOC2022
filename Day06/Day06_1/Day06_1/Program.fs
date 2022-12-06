let rec allDifferent (chars:string): bool =
    match chars.Length with
    | 0 | 1 -> true
    | _ -> (allDifferent chars[1..]) && not (chars[1..].Contains chars[0])

let rec findMarker (chars: string) initPos: int =
    if (allDifferent chars[initPos..initPos+3]) then
        initPos + 4
    else
        findMarker chars (initPos+1)

printfn "Resultado: %d" ((System.IO.File.ReadAllText("input.txt"), 0) ||> findMarker)
