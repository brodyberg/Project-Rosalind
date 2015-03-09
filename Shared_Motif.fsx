#load "Fasta.fs"
#load "FastaCorpora.fs"

open Rosalind

// module Tracker =

type Tracker = 
    { x: int;
      y: int;
      Substrings: Set<string>; 
      Grid: Map<int * int, int>; } 
    with
    member this.MarkMatch () =
        let grid = this.Grid
        let originalValue = 
            let key = (this.x - 1, this.y - 1)

            if grid.ContainsKey(key)
            then grid.[key]
            else 0

        let grid' = grid.Add((this.x, this.y), originalValue + 1); 
            
        { this with Tracker.Grid = grid'; }

    static member AddSubstring (template:string) tracker = 
        let value = tracker.Grid.[(tracker.x,tracker.y)]
        let start = tracker.x - value  + 1
        let end' = value + start - 1
                            
        let theSlice = template.[start..end']

        let x = template.Insert(start, "[")
        let y = x.Insert(end' + 2, "]")

        //    printfn "(value: %d) slicing %s from %d to %d which is: %s" value right start end' theSlice
        printfn "(value: %d) slicing: %s which is: %s" value y theSlice

        { tracker with Tracker.Substrings = (tracker.Substrings.Add theSlice) }

    member this.PrintGrid (str:string) =
        printfn "----------"
        seq { for x in 0 .. str.Length -> x }
        |> Seq.iteri
            (fun x item -> 
                seq { for y in 0 .. str.Length -> 
                        let key = (x, y)

                        if this.Grid.ContainsKey key
                        then this.Grid.[key]
                        else 0 }
                |> Seq.iter
                    (fun item -> printf "%d " item)
                printfn ">")

let trackerDefault = 
    { Tracker.x = 0;
      Tracker.y = 0;
      Tracker.Substrings = Set.empty; 
      Tracker.Grid = Map.empty; }

let compareTwo left (right:string) =
    let iteration = ref 0

    left
    |> Seq.fold
        (fun outerTracker outerItem ->
            let interimTracker = 
                right
                |> Seq.fold
                    (fun innerTracker innerItem ->
                        innerTracker
                        // optionally mark match in grid
                        |> (fun (tracker:Tracker) -> 
                            if outerItem = innerItem 
                            then tracker.MarkMatch()
                            else tracker)
                        // optionally add substring to tracker set
                        |> (fun tracker -> 
                            if outerItem = innerItem
                            then 
                                Tracker.AddSubstring right tracker
                            else tracker)
                        // walk tracker to next location column
                        |> (fun tracker -> 
                            let tempTracker = { tracker with Tracker.x = tracker.x + 1; }
                            tempTracker.PrintGrid right

                            if !iteration % right.Length = 0
                            then printfn "NEW ROW iteration: %d %c =? %c" !iteration outerItem innerItem
                            else printfn "iteration: %d %c =? %c" !iteration outerItem innerItem
                            
                            iteration := !iteration + 1
                            tempTracker))
                    { outerTracker with Tracker.x = 0; }
            // walk tracker to next location row
            { interimTracker with Tracker.y = interimTracker.y + 1; })
        trackerDefault

let test1 = "abbcc"
let test2 = "dbbcc"
// b bb bbc bbcc c cc

let result = compareTwo test1 test2

let result' = compareTwo "a" "a"
result'.PrintGrid "a"

let result2 = compareTwo "ab" "xb"
result2.PrintGrid "ab"

let result'' = compareTwo "xbb" "xbb"
result''.Substrings.Contains("bb") = true
result''.PrintGrid "xbb"

let result''' = compareTwo "bbx" "bbx"
result'''.Substrings.Contains("bb") = true
result''.PrintGrid "bbx"
// set ["b"; "bb"; "bbx"];

