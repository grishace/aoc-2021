open System
open System.IO

[<EntryPoint>]
let main _ =

    let input =
        File.ReadAllLines "input.txt"
        |> Array.map Int32.Parse

    let res1 inp =
        inp
        |> Array.pairwise
        |> Array.filter (fun a -> snd a > fst a)
        |> Array.length

    printfn "Part 1: %i" (res1 input)

    let inp2 =
        input
        |> Array.windowed 3
        |> Array.map Array.sum

    printfn "Part 2: %i" (res1 inp2)

    0