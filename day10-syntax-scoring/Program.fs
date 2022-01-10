open System
open System.IO

let input = File.ReadAllLines "input.txt"

let obs = [| '('; '['; '{'; '<' |]
let cbs = [| ')'; ']'; '}'; '>' |]

let corrupted s =
    let (r, x) = s |> Seq.fold (fun (is, ws) c -> 
                    let oi = Array.IndexOf(obs, c)
                    if oi >= 0
                        then (oi :: is, ws)
                        else 
                          let ci = Array.IndexOf(cbs, c)
                          match is with
                            | [] -> ([], ws)
                            | [h] -> if h = ci then ([], ws) else (is, ci :: ws)
                            | h :: t -> if h = ci then (t, ws) else (is, ci :: ws)

                    ) ([], [])
    match x |> List.rev with
    | [] -> -1, r
    | [h] | h :: _ -> h, r

let c = input |> Array.map corrupted

let res1 = 
    c
    |> Array.filter (fun (x, _) -> x >= 0)
    |> Array.sumBy (fun (x, _) -> 
        match x with
        | 0 -> 3
        | 1 -> 57
        | 2 -> 1197
        | 3 -> 25137
        | _ -> failwith "Unexpectd index"
    )

printfn "Part 1: %i" res1

let res2' = 
    c 
    |> Array.filter (fun (x, _) -> x < 0)
    |> Array.map (fun (_, r) -> r |> List.fold (fun m x -> m * 5L + (int64 x + 1L)) 0L)
    |> Array.sort

let res2 = res2'.[res2'.Length/2]

printfn "Part 2: %i" res2