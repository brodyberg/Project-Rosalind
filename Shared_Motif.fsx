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

// |> (fun (head, tail) -> printBoth head tail)

// we need a list, so we can just first and rest

