open System
open System.IO

let input = 
    File.ReadAllLines "input.txt"
    |> Array.map Array.ofSeq
    |> Array.map (fun a -> a |> Array.map (fun c -> (int c - int '0', false)))

let adjacent' i j =
    [|
        if i-1 >= 0 then yield (i-1, j)
        if i+1 < input.Length then yield (i+1, j)
        if j-1 >= 0 then yield (i, j-1)
        if j+1 < input.[i].Length then yield (i, j+1)
    |]

let adjacent i j = adjacent' i j |> Array.map (fun (i, j) -> input.[i].[j])

let minadjacent i j = adjacent i j |> Array.forall (fun (x, _) -> fst(input.[i].[j]) < x)

let res1 =
    [|
        for i in 0..input.Length-1 do
            for j in 0..input.[i].Length-1 do
                if minadjacent i j then yield fst(input.[i].[j]) + 1
    |] |> Array.sum

printfn "Part 1: %i" res1

let step (i, j) = fst input.[i].[j] <> 9 && not (snd input.[i].[j])

let basin i j =
    let rec basin' i j =
        seq {
          if step (i, j) then  
            input.[i].[j] <- fst input.[i].[j], true
            yield (i, j)
            for (i', j') in adjacent' i j |> Array.filter step do yield! basin' i' j'
        }
    basin' i j |> Seq.length

let basins =
    [|
        for i in 0..input.Length-1 do
            for j in 0..input.[i].Length-1 do
                if step (i, j) then yield basin i j
    |]

let largest = basins |> Array.sortDescending |> Array.take 3
printfn "Part 2: %i" (largest |> Array.fold (*) 1)

