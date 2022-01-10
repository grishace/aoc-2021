open System
open System.IO
open System.Collections.Generic

let splitOptions = StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries
let split (s: string) = s.Split(' ', splitOptions)

let counts =
    File.ReadAllLines "input.txt"
    |> Array.map (fun s -> s.Substring(s.IndexOf('|')+1))
    |> Array.collect split
    |> Array.fold (fun s st -> match st.Length with | 2 | 3 | 4 | 7 -> s + 1 | _ -> s) 0

printfn "Part 1: %i" counts

let result =
    File.ReadAllLines "input.txt"
    |> Array.map(fun s -> 
                let line = s.Split('|', splitOptions)
                line |> Array.head, line |> Array.tail |> Array.head)
    |> Array.map(fun (segments, digits) ->
        let normalize x = x |> split |> Array.map (fun s -> s |> Seq.sort |> String.Concat)
        let m = normalize segments
        let findLen x = m |> Array.find (fun s -> s.Length = x) 

        let r = [|
            None                // 0 m069
            Some (findLen 2)    // 1
            None                // 2 m235
            None                // 3 m235
            Some (findLen 4)    // 4
            None                // 5 m235
            None                // 6 m069
            Some (findLen 3)    // 7
            Some (findLen 7)    // 8
            None                // 9 m069
        |]

        let overlap (s1: string) (s2: string option) = 
            let s = HashSet<char>(s1)
            match s2 with
            | Some x -> 
                s.IntersectWith(x)
                s.Count
            | _ -> 0
        
        let m235 = m |> Array.filter (fun s -> s.Length = 5)
        let m069 = m |> Array.filter (fun s -> s.Length = 6)

        if      overlap m235.[0] r.[4] = 2 then r.[2] <- Some m235.[0]
        else if overlap m235.[1] r.[4] = 2 then r.[2] <- Some m235.[1]
        else if overlap m235.[2] r.[4] = 2 then r.[2] <- Some m235.[2]
        
        if      overlap m235.[0] r.[4] = 3 && overlap m235.[0] r.[7] = 3 then r.[3] <- Some m235.[0]
        else if overlap m235.[1] r.[4] = 3 && overlap m235.[1] r.[7] = 3 then r.[3] <- Some m235.[1]
        else if overlap m235.[2] r.[4] = 3 && overlap m235.[2] r.[7] = 3 then r.[3] <- Some m235.[2]

        if      overlap m235.[0] r.[4] = 3 && overlap m235.[0] r.[7] = 2 then r.[5] <- Some m235.[0]
        else if overlap m235.[1] r.[4] = 3 && overlap m235.[1] r.[7] = 2 then r.[5] <- Some m235.[1]
        else if overlap m235.[2] r.[4] = 3 && overlap m235.[2] r.[7] = 2 then r.[5] <- Some m235.[2]                

        if      overlap m069.[0] r.[4] = 4 then r.[9] <- Some m069.[0]
        else if overlap m069.[1] r.[4] = 4 then r.[9] <- Some m069.[1]
        else if overlap m069.[2] r.[4] = 4 then r.[9] <- Some m069.[2] 

        if      overlap m069.[0] r.[4] = 3 && overlap m069.[0] r.[7] = 3 then r.[0] <- Some m069.[0]
        else if overlap m069.[1] r.[4] = 3 && overlap m069.[1] r.[7] = 3 then r.[0] <- Some m069.[1]
        else if overlap m069.[2] r.[4] = 3 && overlap m069.[2] r.[7] = 3 then r.[0] <- Some m069.[2] 

        if      overlap m069.[0] r.[4] = 3 && overlap m069.[0] r.[7] = 2 then r.[6] <- Some m069.[0]
        else if overlap m069.[1] r.[4] = 3 && overlap m069.[1] r.[7] = 2 then r.[6] <- Some m069.[1]
        else if overlap m069.[2] r.[4] = 3 && overlap m069.[2] r.[7] = 2 then r.[6] <- Some m069.[2]      

        let rd = r |> Array.mapi (fun i x -> match x with Some y -> y | _ -> failwith $"{i}???")
        let d =  normalize digits |> Array.map (fun x -> Array.IndexOf(rd, x))

        d.[0] * 1000 + d.[1] * 100 + d.[2] * 10 + d.[3]
    )
    |> Array.sum

printfn "Partt 2: %i" result

