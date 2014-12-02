type BaseCounts = { A: int; G: int; C: int; T: int; }

let printRecord (r:BaseCounts) = printfn "%d %d %d %d" r.A r.C r.G r.T
let greaterThan1000OrPass (dna:string) = if dna.Length > 1000 then "" else dna
let stringAsSeq (str:string) = seq { for c in str -> c }

"TCATACTCAAGATATTGATCCTTAGGGGCGCATAGCACAAGGAGGACGCGTAAAGAACCGTGAACTATTTCCGCGTAGTCCTTCTTCTGGCCTTTCCGATGCCCCATCTGTCCCGCGACGTTGCCGCAGGTTACTCGTTGTTAGAAACAGATCCATTACGGATCCCGCTGAATGCGAAACAGGAAGTCCCAACGTGTACTTCAGACGTGCTGTGGTCCGCTACCGCTTTGTAGACTGACCATGGAGCATTTACTGCGCCCCCCCTGACTCGAATAACCGGACCGCTAGACGAACATGGATCCTTGAAGACTCCTGAGGACTATGTTCAGGCAGTCCTAAGAGGTAGCTCACAAGTCTTGGTCTATCGGTCAACCACATTGTCTAAGTTGGGGGCTGCTAGGAGAAAACGTTAAGACACGACGCGCCAAAGAGCTAGCAGGCGTCGTCTCACTGGGGTGTAGAGTACACGTAAGGTTGAGCAGGTGAGCGCGAGCTTAGACTTCCACTAATTGAGGTCAGACTTCGGATTTAGGGGGAAGGGTATATTTCGTTGACATCTTAACTGGCCTCTTCAGTTTGGTGAATGCGGGCGTAAGAATTCGCCGAACCTTTCGAGAAGCCAGAAATACACGGTGTTCGCCGGAGGCCGAACATTCTAAGTAACCAATCTCCTAGGTACAGAGTTCGGTGACCCGACGTAAGGTTTAAACCAACAGTATTCAACTAAGAACCTAGTTTTCCAAAATTCGCGTGTACTTAAGCTGCTGCTTCTGGCCATGTATCAGACGCTCGTATCTGATGCAGGTA"
|> greaterThan1000OrPass
|> stringAsSeq
|> Seq.fold         
        (fun acc item -> 
            match item with
            | 'A' -> { acc with A = acc.A + 1 }
            | 'C' -> { acc with C = acc.C + 1 }
            | 'G' -> { acc with G = acc.G + 1 }
            | 'T' -> { acc with T = acc.T + 1 }
            | _ -> acc)
        { A = 0; G = 0; C = 0; T = 0; }
|> printRecord

// 204 198 207 198
