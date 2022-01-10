open System.IO
open System.Text.RegularExpressions

let rx = Regex("(\\d+),(\\d+) -> +(\\d+),(\\d+)")

let lines = 
    File.ReadAllLines "input.txt"
    |> Array.filter rx.IsMatch
    |> Array.map (fun s ->
        let m = rx.Match(s)
        ((int m.Groups.[1].Value, int m.Groups.[2].Value), (int m.Groups.[3].Value, int m.Groups[4].Value))
    )

// Obviously, this should be refactred from imperative to functional

let l1 = lines |> Array.filter (fun ((x1, y1), (x2, y2)) -> x1 = x2 || y1 = y2)
let mx = [| for ((x1, y1), (x2, y2)) in l1 do yield! [| x1; y1; x2; y2 |] |] |> Array.max

let createField () = [| for y in 0..mx do yield [| for x in 0..mx do yield 0 |] |]

let mutable field = createField()
for ((x1, y1), (x2, y2)) in l1 do
    if x1 = x2
        then
            let y1' = min y1 y2
            let y2' = max y1 y2
            for y in y1'..y2' do field.[y].[x1] <- field.[y].[x1] + 1
        else
            if y1 = y2
                then
                    let x1' = min x1 x2
                    let x2' = max x1 x2
                    for x in x1'..x2' do field.[y1].[x] <- field.[y1].[x] + 1

printfn "Part 1: %i" (
    [| for f in field do yield! f |] |> Array.filter (fun f -> f > 1) |> Array.length)

let l2 = lines
field <- createField()
for ((x1, y1), (x2, y2)) in l2 do
    let y1' = min y1 y2
    let y2' = max y1 y2
    if x1 = x2
        then
            for y in y1'..y2' do field.[y].[x1] <- field.[y].[x1] + 1
        else     
            if y1 = y2
                then
                    let x1' = min x1 x2
                    let x2' = max x1 x2   
                    for x in x1'..x2' do field.[y1].[x] <- field.[y1].[x] + 1
                else
                    let mutable x1'' = x1
                    let mutable x2'' = x2
                    let mutable y1'' = y1
                    let mutable y2'' = y2

                    if x1 > x2 
                        then
                            x1'' <- x2
                            x2'' <- x1
                            y1'' <- y2
                            y2'' <- y1  

                    for x in x1''..x2'' do
                        let dx = x - x1''
                        let y = if y2'' > y1'' then y1'' + dx else y1'' - dx
                        field.[y].[x] <- field.[y].[x] + 1

printfn "Part 2: %i" (
    [| for f in field do yield! f |] |> Array.filter (fun f -> f > 1) |> Array.length)