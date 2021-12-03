open System
open System.IO

[<EntryPoint>]
let main _ =
    let diag = File.ReadAllLines "input.txt"

    let bits (log: string[]) n =
        log
        |> Array.map (fun b -> b.[n])
        |> Array.sort
        |> Array.groupBy id
        |> Array.map (fun (_, cc) -> cc.Length)

    let extract (log: string[]) (cmp: int[] -> bool) =
        [0..11]
        |> List.map (fun  n -> bits log n)
        |> List.map (fun b -> if (cmp b) then 0 else 1)
        |> List.rev

    let bin (a: #seq<int>) =
        a
        |> Seq.fold (fun (s, p) b -> (s + p * b, p * 2)) (0, 1)
        |> fst

    let gamma = extract diag (fun b -> b.[0] > b.[1]) |> bin
    let epsilon = extract diag (fun b -> b.[0] < b.[1]) |> bin

    printfn "Part 1: %i" (gamma * epsilon)

    let filter (log: string[]) (cmp: int[] -> bool) =
        let rec filter' b (log: string[]) =
            match log with
            | [|a|] -> a
            | a'   ->
                a'
                |> Array.map (fun s -> s, bits log b)
                |> Array.filter (fun (s, x) -> s.[b] = if (cmp x) then '0' else '1')
                |> Array.map fst
                |> filter' (b+1)
        filter' 0 log

    let bin' s = s |> Seq.rev |> Seq.map (fun c -> match c with '1' -> 1 | _ -> 0) |> bin
    let oxy = filter diag (fun b -> b.[0] > b.[1]) |> bin'
    let co2 = filter diag (fun b -> b.[0] <= b.[1]) |> bin'

    printfn "Part 2: %i" (oxy * co2)

    0