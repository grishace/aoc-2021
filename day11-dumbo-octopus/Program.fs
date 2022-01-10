open System
open System.IO

let input = 
    File.ReadAllLines "input.txt"
    |> Array.map Array.ofSeq
    |> Array.map (fun a -> a |> Array.map (fun c -> int c - int '0', false))

let adjacent i j =
    [|
        for di in -1..1 do
            for dj in -1..1 do
                if not(di = 0 && dj = 0)
                && i+di >= 0 && i+di < input.Length
                && j+dj >= 0 && j+dj < input.[i+di].Length then
                    yield (i+di, j+dj)
    |]

let inc () =
    for i in 0..input.Length-1 do
        for j in 0..input.[i].Length-1 do
            input.[i].[j] <- fst input.[i].[j] + 1, false

let step () =
    let rec step' i j =
        if fst input.[i].[j] > 9 && not(snd input.[i].[j])
            then
                input.[i].[j] <- fst input.[i].[j], true
                let f = adjacent i j
                for (ai, aj) in f do input.[ai].[aj] <- fst input.[ai].[aj] + 1, snd input.[ai].[aj]
                for (ai, aj) in f do step' ai aj

    for i in 0..input.Length-1 do
        for j in 0..input.[i].Length-1 do
            step' i j    

let reset () =
    let mutable flash' = 0
    for i in 0..input.Length-1 do
        for j in 0..input.[i].Length-1 do
            if snd input.[i].[j]
                then
                    flash' <- flash' + 1
                    input.[i].[j] <- 0, false
    flash'

let mutable flash = 0
let size = input.Length * input.[0].Length

for s in 1..100 do
    inc()
    step()
    let cnt = reset()
    if cnt = size then printfn "Part 2: %i" s
    flash <- flash + cnt
    
printfn "Part 1: %i" flash

let mutable cnt = 0
let mutable s = 101
while cnt <> size do
    inc()
    step()
    cnt <- reset()
    if cnt <> size then s <- s + 1

printfn "Part 2: %i" s
