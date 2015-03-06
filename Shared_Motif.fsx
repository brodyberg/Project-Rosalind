#load "Fasta.fs"
#load "FastaCorpora.fs"

open Rosalind

let printBoth head tail = 
    printfn "head: %A tail: %A" head tail

FastaCorpora.exampleLong
|> Fasta.strToFastaSeq
|> List.ofSeq
|> (fun lst -> (lst.Head, lst.Tail))

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

