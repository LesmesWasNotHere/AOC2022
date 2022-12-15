//I used a recursive A* 

let heuristic (map:char array2d) ((y,x): (int*int)) ((desty,destx): (int*int)) =
    abs (desty-y) + abs (destx-x)

type node = 
    struct
        new (position:(int*int), gvalue:int, fvalue:int) = {
            pos = position;
            g = gvalue;
            f = fvalue;
            parent = None;
        }

        new (position:(int*int), gvalue:int, fvalue:int, theparent:node) = {
            pos = position;
            g = gvalue;
            f = fvalue;
            parent = Some(theparent);            
        }
        val pos:(int*int)
        val g:int
        val f:int
        val parent: node option
    end

let positionValue (map:char array2d) (y,x): char =
    let value = map[y,x]
    match value with
            |'S' -> 'a'
            |'E' -> 'z'
            | _  -> value

let createNode (map:char array2d) (pos:(int*int)) (dest:(int*int)) (parent:node) : node =
    let g = parent.g + 1
    new node( pos, g, g + (heuristic map pos dest), parent)
    
let canGoTo (map:char array2d) originPos (destPos:(int*int)) =
    let (dpY, dpX) = destPos
    dpY >= 0 && dpY < map.GetLength(0) && dpX >= 0 && dpX < map.GetLength(1) && (int(positionValue map originPos) - int(positionValue map destPos)) >= -1

let generateChilds (map:char array2d) (parent:node) destPos: node list =
    let (parentY, parentX) = parent.pos
    let childPos = [(parentY,parentX+1); (parentY,parentX-1); (parentY+1,parentX); (parentY-1,parentX)]
    [
        for pos in childPos do if (canGoTo map parent.pos pos) then yield (createNode map pos destPos parent)
    ]   
     
let choseChildCondition (child:node) (alist: node list) =
    not (List.exists (fun (x:node)-> x.pos = child.pos && x.f <= child.f) alist)

let rec searchPath (map:char array2d) (openList:node list) (closedList:node list) (destPos:(int*int)): node option =
    if (openList.IsEmpty) then
        None
    else
        let sortedOpenList = List.sortBy (fun (x:node)->x.f) openList
        let first = sortedOpenList.Head
        if (first.pos = destPos) then
            Some(first)
        else
            let childs = generateChilds map first destPos
            let filteredChilds = List.filter (fun x -> (choseChildCondition x openList) && (choseChildCondition x closedList)) childs
            searchPath map (List.append sortedOpenList.Tail filteredChilds) (List.append closedList [first]) destPos

let rec composeResultPath (endNode:node option) : node list =
    match endNode with
        |None -> []
        | _ -> List.append (composeResultPath endNode.Value.parent) [endNode.Value]

let findPosition (c:char) (a:char array2d): (int*int) list =
    [for y in [0..a.GetLength(0)-1] do
        for x in [0..a.GetLength(1)-1] do
            if (a[y,x] = c) then
                yield (y,x)
    ]

let map = System.IO.File.ReadAllLines("input.txt") |> Array.map (fun x -> x.ToCharArray()) |> array2D
let startPositions = List.append (findPosition 'S' map) (findPosition 'a' map)
let destPosition = (findPosition 'E' map).Head
let startNodes = List.map (fun x -> new node(x, 0, heuristic map x destPosition)) startPositions
let resultNode = searchPath map startNodes [] destPosition
let resultPath = composeResultPath resultNode

printfn "Result %d" (resultPath.Length-1)