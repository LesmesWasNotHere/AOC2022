open System.Text.RegularExpressions
//70000000
//30000000
//5649896

let lines = System.IO.File.ReadAllLines "input.txt"
let mutable currIndex = 0

type directory =
    struct
        new (dn:string, dr: directory list, fl: int list) = {dirName = dn; directories = dr; files = fl}
        val dirName:string
        val directories: directory list
        val files: int list
    end

let rec calcFileSizes (dir:directory): int =
    (List.sum dir.files) + (List.map calcFileSizes dir.directories |> List.sum)

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
    List.append [for d in dir.directories do yield! calcDirSizes d] [calcFileSizes dir]

let rootDirectory = parseDir()
let diskUsage = calcFileSizes rootDirectory
let spaceToFree = 30000000 - (70000000 - diskUsage)

printfn "Disk usage: %d" diskUsage
printfn "Space to free %d" spaceToFree
printfn "Result %d" (rootDirectory |> calcDirSizes |> List.filter (fun x -> x >= spaceToFree) |> List.sort).Head