open System.Text.RegularExpressions

let lines = System.IO.File.ReadAllLines "input.txt"
let mutable currIndex = 0

type directory =
    struct
        new (dn:string, dr: directory list, fl: int list) = {dirName = dn; directories = dr; files = fl}
        val dirName:string
        val directories: directory list
        val files: int list
    end

let rec parseDir () : directory =
    let dirName = lines[currIndex][5..]
    let mutable directories: directory list = []
    let mutable files = []

    currIndex<-currIndex+1

    while (currIndex < lines.Length && lines[currIndex] <> "$ cd ..") do
        let line = lines[currIndex]
        if line.StartsWith("$ cd ") then
            directories <- List.append directories [ parseDir() ]
        else
            let m = Regex("^\d+").Match(line)
            if (m.Success) then
                files <- List.append files [int m.Groups[0].Value]
            currIndex <- currIndex + 1

    currIndex <- currIndex + 1

    new directory(dirName, directories, files)

let rec calcDirSizes (dir:directory): int list =
    let dirSizes = List.map calcDirSizes dir.directories |> List.concat
    let dirSize = (List.sum dir.files) + (List.sum dirSizes)
    List.append [dirSize] dirSizes

printfn "Result %d" (parseDir() |> calcDirSizes |> List.filter (fun x -> x <= 100000)  |> List.sum)