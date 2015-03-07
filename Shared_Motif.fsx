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

//
//let compareDown (acc:Tracker) (itemOuter:char) (itemInner:char) =
//    let rowResult = 
//        right
//        |> Seq.fold 
//            (fun (accInner:Tracker) itemInner ->
//                compareAcross accInner itemOuter itemInner
//
//                // now, what can do tell about index and max?
//            )
//            acc
//
//    { rowResult with Tracker.y = rowResult.y + 1; }                    
//
//let compareTwo (left:string) (right:string) =
//    let substringTracker =
//        left
//        |> Seq.fold
//            (fun (accOuter:Tracker) itemOuter -> 
//                let columnResult = 
//                    compareDown accOuter itemOuter
////                let rowResult = 
////                    right
////                    |> Seq.fold 
////                        (fun (accInner:Tracker) itemInner ->
////                            compareAcross accInner itemOuter itemInner
////
////                            // now, what can do tell about index and max?
////                        )
////                        accOuter
////
////                { rowResult with Tracker.y = rowResult.y + 1; }                    
//            )
//            defaultTracker
//            //            right
//
//    ""

//let compareTwo1 (left:string) (right:string) =
//    let substringTracker =
//        left
//        |> Seq.fold
//            (fun (accOuter:SubstringTracker) itemOuter -> 
//                right
//                |> Seq.fold 
//                    (fun (accInner:SubstringTracker) itemInner ->
//                        if itemOuter = itemInner 
//                        then 
//                            let originalValue = 
//                                if accInner.Map.ContainsKey((accInner.x - 1, accInner.y - 1))
//                                then accInner.Map.[(accInner.x - 1, accInner.y - 1)]
//                                else 0
//                            
//                            let updatedMap = accInner.Map.Add((accInner.x, accInner.y), originalValue + 1); 
//            
//                            { accInner with SubstringTracker.Map = updatedMap; }
//                        else
//                            // MUST INCREMENT X Y INDEX MAX
//                            accInner
//                    )
//                    accOuter              
//            
//            
//            )
//            defaultSubstringTracker
//            //            right
//
//    ""

compareTwo test1 test2

//let substringTracker = 
//    test1
//    |> Seq.fold 
//        (fun acc item -> 
//        
//        )



// we need to walk our index, max, x and y
let substringTracker = 
    Seq.fold2
        (fun (acc:SubstringTracker) left right -> 
            printfn "Examining: %c %c" left right

            if left = right
            then 
                let originalValue = 
                    if acc.Map.ContainsKey((acc.x - 1, acc.y - 1))
                    then acc.Map.[(acc.x - 1, acc.y - 1)]
                    else 0

                let updatedMap = acc.Map.Add((acc.x, acc.y), originalValue + 1); 
            
                { acc with SubstringTracker.Map = updatedMap; }
            else acc)
        defaultSubstringTracker
        test1
        test2



//
//
//Seq.fold2
//    (fun (acc:SubstringTracker) left right -> 
//        if left = right
//        then 
//            let updatedMap = acc.Map.Add((acc.x, acc.y), (acc.Map.[(acc.x - 1, acc.y - 1)]) + 1); 
//            
//            { acc with SubstringTracker.Map = updatedMap; }
//        else acc
//            //acc
////            (acc.Map.Add((acc.x, acc.y), (acc.Map.[(acc.x - 1, acc.y - 1)]) + 1))
////            { }
//            
//            
//  //           acc.Map.Add((acc.x, acc.y), (acc.Map.[(acc.x - 1, acc.y - 1)]) + 1)
//
//
//    )
//    defaultSubstringTracker
////    { 0; 0; 0; 0; Map.empty; }
//    test1
//    test2

//            acc.Map.Add(((acc.x), (acc.y)), (acc.Map.[((acc.x) - 1, (acc.y) - 1)]) + 1)


// we need to ensure that future substrings are within the 
// existing set of substring matches we can NEVER ADD to the
// set of substring matches
// but it goes both ways, we also need to require that nothing from 
// the existing set survives unless it is duplicated in the 
// newly found list of substrings
// In conclusion, this is just a hack way of saying
//  "Set intersection" right?

let (_,_,resultMap) =
    Seq.fold2
        (fun (acc:int * int * Map<int * int, int>) left right -> 
            let (x, y, map) = acc

            if left = right 
            then (x + 1, y + 1, map.Add((x, y), 1))
            else (x + 1, y + 1, map))
        (0, 0, Map.empty)
        first.DNA
        dummyFasta.DNA

resultMap.Count    
//first.DNA
//Seq.fold2 (fun acc left right -> acc::(left + right)) [] first.DNA first.DNA 
// Seq.fold2 (fun acc left right -> left + acc) [] (List.ofSeq first.DNA) first.DNA 



Seq.fold2 
    (fun acc left right -> acc + (left + right)) 
    "" 
    [|"f"; "o"; "o"; |] 
    [|"b"; "a"; "r"; |]

|> Seq.iter (fun item -> printfn "blerg: %c" item) 

// dynamic programming in a functional manner

// arrays are mutable!?
let x = [| 0; 2; 1; |]
x.[0] <- 4

x

// part of my goal is to show that we can do this without
// indexing and potential off by one errors
// is there a way we can fold with the accumulator being
// something immutable?

// The point is sort of less about immutability maybe
// and more about the "whereness" of what we update
// the whole business of updating back or up or down
// is the tough part that seems to insist on keeping
// around all sorts of indices

//open Microsoft.FSharp.Collections.Array2D

let n = Array2D.zeroCreate<int> 10 10
Array2D.zeroCreate<int> 10 10

// make sure we can reach the right size
Array2D.zeroCreate<int> 1000 1000

// so our accumulator will be: 
Array2D.zeroCreate<int> 1000 1000

let x = Map.empty
x.Add("key", "value")
x

let y = x.Add("key", "value")
let z = y.Add("key", "value1")

let u = Map.empty
let v = u.Add((0,0), 0)
let w = v.Add((0,1), 0)
w.[(0,0)]
let r = w.Add((0,2), 2)
r.[(0,2)]

// how would you iterate through this thing?
// iteri?

// |> (fun (head, tail) -> printBoth head tail)

// we need a list, so we can just first and rest

