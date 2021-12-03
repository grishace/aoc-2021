open System
open System.IO
open System.Text.RegularExpressions

type Command =
    | Forward of int
    | Up of int
    | Down of int

let rx = new Regex("(forward|up|down)\\s+(\\d+)")

[<EntryPoint>]
let main _ =

    let commands =
        File.ReadAllLines "input.txt"
        |> Array.map (fun cmd ->
                let m = rx.Match(cmd)
                if not(m.Success)
                    then None
                    else
                        let x = Int32.Parse m.Groups.[2].Value
                        match m.Groups.[1].Value with
                        | "forward" -> Some(Forward x)
                        | "up"      -> Some(Up x)
                        | "down"    -> Some(Down x)
                        | _         -> None)
        |> Array.choose id

    let res1 =
        commands
        |> Array.fold (fun (x, y) c ->
            match c with
            | Forward n -> (x + n, y)
            | Up n      -> (x, y - n)
            | Down n    -> (x, y + n)
        ) (0, 0)

    printfn "Part 1: %i" ((fst res1) * (snd res1))

    let res2 =
        commands
        |> Array.fold (fun (x, y, a) c ->
            match c with
            | Forward n -> (x + n, y + a * n, a)
            | Up n      -> (x, y, a - n)
            | Down n    -> (x, y, a + n)
        ) (0, 0, 0)

    printfn "Part 2: %i" (match res2 with (x, y, _) -> x * y)

    0