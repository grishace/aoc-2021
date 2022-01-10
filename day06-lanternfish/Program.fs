open System
open System.IO

let fishes () = 
    let idx =
        File.ReadAllLines "input.txt"
        |> Array.head
        |> (fun s -> s.Split(',', StringSplitOptions.TrimEntries)) 
        |> Array.groupBy int
        |> Array.map (fun (k, v) -> k, v.LongLength)
        |> Map.ofArray

    let cnt n = 
        match idx.TryGetValue n with
        | (false, _) -> 0L
        | (_, x)     -> x

    [| for i in 0..8 -> cnt i |]

let generation (f: int64 array): int64 array =
    [|
        f.[1]
        f.[2]
        f.[3]
        f.[4]
        f.[5]
        f.[6]
        f.[7] + f.[0]
        f.[8]
        f.[0]
    |]

let rec life n f =
    match n with
    | 0 -> f
    | s -> life (s-1) (generation f)

printfn "Part 1: %i" (life 80 (fishes()) |> Array.sum)
printfn "Part 2: %i" (life 256 (fishes()) |> Array.sum)