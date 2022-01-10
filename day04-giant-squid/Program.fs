open System
open System.IO
open System.Linq
open System.Collections.Generic

type Board = (int * bool) [] []

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines "input.txt"
    
    let draws = input.[0] .Split ',' |> Array.map (int) 

    let createBoards () : Board [] = [|
        let mutable boardStart = 2
        while boardStart < input.Length do
            yield [|
                for d in 0..4 do
                    yield input.[boardStart + d].Split(' ', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
                    |> Array.map (fun x -> int x, false)
                boardStart <- boardStart + 6
            |]
    |]

    let draw n (board: Board) =
        for i in 0..board.Length-1 do
            for j in 0..board.[i].Length-1 do
                if fst board.[i].[j] = n then board.[i].[j] <- n, true

        let anyRow (b: Board) = b.Any(fun row -> row.All snd)
        anyRow board || anyRow (board |> Array.transpose)

    let score n (board: Board) =
        let mutable sum = 0
        for i in 0..4 do
            for j in 0..4 do
                if not(snd board.[i].[j])
                    then sum <- sum + fst board.[i].[j]
        sum * n

    let doPart1 (boards: Board []) =
        let mutable di = 0
        let mutable win  = false

        while di < draws.Length && not win do
            let mutable bi = 0
            while bi < boards.Length && not win do
                win <- draw draws.[di] boards.[bi]
                if win
                    then
                        printfn "Part 1: %i" (score draws.[di] boards.[bi])
                    else
                        bi <- bi + 1
            di <- di + 1

    let doPart2 (boards: Board []) =
        let mutable di = 0
        let mutable wd = 0

        let won = List<int>()

        while di < draws.Length do
            let mutable bi = 0
            while bi < boards.Length  do
                if not(won.Contains bi) && draw draws.[di] boards.[bi]
                    then
                        won.Add bi
                        wd <- draws.[di]
                bi <- bi + 1
            di <- di + 1

        printfn "Part 2: %i" (score wd boards.[won.Last()])

    doPart1 (createBoards())
    doPart2 (createBoards())

    0