#load "Fasta.fs"

open Rosalind

let printBoth head tail = 
    printfn "head: %A tail: %A" head tail

">Rosalind_1
GATTACA
>Rosalind_2
TAGACCA
>Rosalind_3
ATACA"
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


// |> (fun (head, tail) -> printBoth head tail)

// we need a list, so we can just first and rest

