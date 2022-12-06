let rec allDifferent (chars:string): bool =
    match chars.Length with
    | 0 | 1 -> true
    | _ -> (allDifferent chars[1..]) && not (chars[1..].Contains chars[0])

let rec findMarker (chars: string) markerLength initPos : int =
    if (allDifferent chars[initPos..initPos+markerLength-1]) then
        initPos + markerLength
    else
        findMarker chars markerLength (initPos+1) 

printfn "Resultado: %d" ((System.IO.File.ReadAllText("input.txt"), 14, 0) |||> findMarker)
