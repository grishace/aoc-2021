open System
open System.IO

let crabs =
    File.ReadAllLines "input.txt"
    |> Array.head
    |> (fun s -> s.Split(',', StringSplitOptions.TrimEntries)) 
    |> Array.map int

let m1 = crabs 
        |> Array.map (fun x -> crabs |> Array.map (fun y -> abs(x - y)) |> Array.sum)
        |> Array.min

printfn "Part 1: %i" m1

let m2 = seq { 0..crabs |> Array.max } 
        |> Seq.map (fun x -> crabs |> Array.map (fun y -> 
                                                    let n = abs(x - y)
                                                    n * (n + 1) / 2
                                                ) |> Array.sum)
        |> Seq.min

printfn "Part 2: %i" m2
        