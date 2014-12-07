let greaterThan1000OrPass (dna:string) = if dna.Length > 1000 then "" else dna
let stringToSeqOfRNA (str:string) = 
    seq 
        { for c in str -> 
                    match c with 
                    | 'T' -> 'U' // Transpose
                    | _ -> c 
        }

"GAGAGTTTTTAACAACAAATGCTGCGCTAAATAACAGGACGACCCAGTCGGTGCATGGCTTGTAACAGCTAGTTCGAGACAACGGGGCGCAGCTTGCGAATCTGGCCAATTTAAGGGGAGTCCCCGACGAGTCGTGGCCTTTGCCCTTCCACGTAGCTGTTAACTAGACTCGTGGGGCGAACCTCCGGAAACCTGGCAGGGAAAGATAGACCTTTAGATCGAACCATTATTTCCAAGAGCAACACCAGTTGAACTAATCCCCGAGCGCAAGGCATAGTAGTGAGGCCCACACTGTCGTGACCAAGGTCGGGTCGTCGTCAAGGATTGTAATGTGTACGGATGGACGGATACAGCTATAAAATGCTAGGGTAGGACAAGACCCCAGGTGATCACCTTCGCGCAATACTACTTTACTTTGGAGTGGAATTAGCGGCTGCGCCGACGTAAATAGGGCATCAGGTACGTTATGGGCTCCTCACGAGCATTTGATTTGTCTGCGAAATGACTTCGTAGAGCGAAGGGCACCTCGTAGCGGTGCAATCGTTTGGAGTCATGGAAAGTGCGTCCAACTCTCCACCGGAATGAACTGCAGGGGCTCCCCTGCTAAGGTAAAATCGAGTGACGTATTCCAACCAGTCCAGGTCGACTCTGCCTGAATAACGGTTACTGATCGGAGACGGCGATAACGAGATGGCAGTCTTGTAGCCTCATAGCGACCCGATGCCGTCCACCCTCGAGCCGGCTCTTCCCTTCTTTCCCTGTTCGCCGGCTTGCCAATTAGGCGGCCGGCATTAACCTCCGGTGTTTCTCTGCAAACTCTCAATTTGGCACCTACACTCTTCTTGTGGACCTTTTTCGTTGCCTGACGACGACAAAAGCGATTGCTATAGGGCATTCTTCACTGAACGAATACCAGACCGGCCGGAAAAGGTGCCAGCGGCGCCAAAACTGCGAAAGGTGTTTATATCGTCGATGCCCTATGGCCAA"
|> greaterThan1000OrPass
|> stringToSeqOfRNA
|> Seq.iter (fun c -> printf "%c" c)