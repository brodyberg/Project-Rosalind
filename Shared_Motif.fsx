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
    { Index: int; 
      Max: int;
      x: int;
      y: int;
      Map: Map<int * int, int>; }

let trackerDefault = 
    { Tracker.Index = 0;
      Tracker.Max = 0;
      Tracker.x = 0;
      Tracker.y = 0;
      Tracker.Map = Map.empty; }

let test1 = "abbcc"
let test2 = "dbbcc"

let substringSet = Set.empty

let printGrid (map:Map<(int * int), int>) =
    printfn "----------"
    seq { for x in 0 .. 5 -> x }
    |> Seq.iteri
        (fun x item -> 
            seq { for y in 0 .. 5 -> 
                    let key = (x, y)

                    if map.ContainsKey key
                    then map.[key]
                    else 0 }
            |> Seq.iter
                (fun item -> printf "%d " item)
            printfn ">")

let compare tracker left right =
    let interimTracker = 

        if left = right 
        then 
            let map = tracker.Map
            let originalValue = 
                let key = (tracker.x - 1, tracker.y - 1)

                if map.ContainsKey(key)
                then map.[key]
                else 0
                            
            let map' = map.Add((tracker.x, tracker.y), originalValue + 1); 
            
            { tracker with Tracker.Map = map'; }
        else
            tracker

    { interimTracker with Tracker.x = interimTracker.x + 1; }

let compareTwo left right =
    left
    |> Seq.fold
        (fun accOuter outerItem ->
            printfn "outerItem: %c" outerItem
            let interimResult = 
                right
                |> Seq.fold
                    (fun accInner innerItem ->
                        printfn "innerItem: %c" innerItem
                        printGrid accInner.Map |> ignore
                        compare accInner outerItem innerItem)
                    { accOuter with Tracker.x = 0; }
            { interimResult with Tracker.y = interimResult.y + 1; })
        trackerDefault

let result = compareTwo test1 test2
result.Map.Count

result.Map.[(6,0)]

