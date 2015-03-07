#load "Fasta.fs"
#load "FastaCorpora.fs"

let dummySecond = 
    ">Rosalind_2970
    GCGAGGTTCGGCATAATAGCAGATGTCGCAAAGGGTAAGTAAACGTTGACGGGGGCGTCC
    CCTCAGCGTAGTAATATTTACCCTTATAGTCAATCAGACAGACGTATAATTACCGAAGGA
    CTTAAGACCTCGACATTCGAAATGCAAGCTCGTAGGACCAACCGGCGTCTGTGCTATTTG
    ATATAGTCTTGTTGCTATAGGGGTCCTGTCCCGACATCCATGGCCCGCTACCCATGCCCA
    TTTCTCTACGCAGGATGGACTCCGGTCTCATTCCTAAAACGCTACGTACAAATTTCATTA
    AGCCAGCGCTTAATGAATGCTGTGAAATACGCTAGGGCAGAGTCTGAGGGCAAGGCGGGG
    GCAGAAGCGCAATGTACGATGACAGGGTGAGCGCCGCGCGGGTATATCATGCTTCAACCA
    GACCCCCCGATCTCTGCTGTTCGGGCAATCCGCGGCATAATGGTGAGCTACGAGAAATCC
    CTCCGCATGTTCAACGTCACATCACTGCTCAATCGGTTTTCGCCATAAACCATTGCGTAG
    TTTAAGGATCACCAGAGGCGACCCATCGAACGTTCGTGGGAGTATCGTTAAAGTACGAAT
    AGCCCGCAGAGCCCCACGACCGAGGTATCACTCGTACAATCCCCGTGGTTCGGCAAGCTT
    ACACAATTATAGACTGCGTTCGCCGATATTTTTTTCGAGTGAACAGTAGCCCTAGTTGTC
    CACGTTACCTGAACCTGGCTGCAACGGCCCTGATCTTCGGCCAGTGGACCCGTAAATATC
    AGTGAGCCCAGAGCTCGCATAGCTGACTTACAGCGGGTTGGGGGGGGATTGTAAGGGGCC
    TGAGGCATACAGTTATTGCTGTGTTAGCAACCCCGTCGTATTCCGGAGCATAAAGTGACT
    CGCGTCGTGTCGTTCATCTTCTGGTATCACCCGGCGCGTATCATAAATACGACGATTCAA
    ATCAGGAAAAATAAAGACCGCAAATAATGGAGAAGTGGCC"

open Rosalind

let printBoth head tail = 
    printfn "head: %A tail: %A" head tail

FastaCorpora.exampleLong
|> Fasta.strToFastaSeq
|> List.ofSeq
|> (fun lst -> (lst.Head, lst.Tail))

let (first, rest) = 
    FastaCorpora.exampleLong
    |> Fasta.strToFastaSeq
    |> List.ofSeq
    |> (fun lst -> (lst.Head, lst.Tail))

let dummyFasta =
    dummySecond
    |> Fasta.strToFastaSeq
    |> Seq.head

type Tracker = 
    { x: int;
      y: int;
      Substrings: Set<string>; 
      Grid: Map<int * int, int>; }

let trackerDefault = 
    { Tracker.x = 0;
      Tracker.y = 0;
      Tracker.Substrings = Set.empty; 
      Tracker.Grid = Map.empty; }

let test1 = "abbcc"
let test2 = "dbbcc"

let printGrid (grid:Map<(int * int), int>) =
    printfn "----------"
    seq { for x in 0 .. 5 -> x }
    |> Seq.iteri
        (fun x item -> 
            seq { for y in 0 .. 5 -> 
                    let key = (x, y)

                    if grid.ContainsKey key
                    then grid.[key]
                    else 0 }
            |> Seq.iter
                (fun item -> printf "%d " item)
            printfn ">")

let updateTracker tracker = 
    let grid = tracker.Grid
    let originalValue = 
        let key = (tracker.x - 1, tracker.y - 1)

        if grid.ContainsKey(key)
        then grid.[key]
        else 0

    let grid' = grid.Add((tracker.x, tracker.y), originalValue + 1); 
            
    { tracker with Tracker.Grid = grid'; }

let compareTwo left (right:string) =
    left
    |> Seq.fold
        (fun accOuter outerItem ->
            let interimResult = 
                right
                |> Seq.fold
                    (fun accInner innerItem ->
                        // optionally mark match in grid
                        let tracker' = 
                            if outerItem = innerItem
                            then updateTracker accInner
                            else accInner

                        let tracker'' = 
                            // optionally add substring to tracker set
                            if outerItem = innerItem
                            then 
                                let start = tracker'.x
                                let end' = tracker'.Grid.[(tracker'.x,tracker'.y)]
                            
                                let theSlice = right.[start..end']

                                { tracker' with Tracker.Substrings = (tracker'.Substrings.Add theSlice) }
                            else
                                tracker'

                        // walk tracker to next location column
                        { tracker'' with Tracker.x = tracker''.x + 1; })
                    { accOuter with Tracker.x = 0; }
            // walk tracker to next location row
            { interimResult with Tracker.y = interimResult.y + 1; })
        trackerDefault
      
let result = compareTwo test1 test2

result.Substrings
printGrid result.Grid
