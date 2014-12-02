let greaterThan1000OrPass (dna:string) = if dna.Length > 1000 then "" else dna
let stringToDNAComplement (str:string) = 
    seq 
        { for c in str -> 
                    match c with 
                    | 'A' -> 'T' 
                    | 'T' -> 'A'
                    | 'C' -> 'G'
                    | 'G' -> 'C'
                    | _ -> c 
        }

"GTTAGTTATGGTTGCAGTCCATCGTTAGTCCTCTTCTATTTGGTCACATCTCGCGTCTGTATCTCATACTATGTTCTTCTCGAAGGTAACTAGAGCGGTTTAGGAGAAGCTTTTGACGACGCTACTTCTTGGACGCTCCTTGGCGTCACCCCATGTAACCAACTCACTGACACTTATCGGCGCATTCTTTATGTTACCTGCGCCGAAGGGTACGTCTTCAGAACTTTAGCTTCCACAATCGCCTTGCCGATGTGACCCCTTCCACCCGTGATACCTGTGCGACTGACTTTTGGTCAGTCCTGAAGGACTCTACTGAATCATAAAACTTCCCGTTTTATGGATTGGTTATACTATAAGGCTGGTTCGCAGAACAACTAGCGTCTGTCACTTAATCCCCCCCATGTATACCGTTACCTAGCCCTCCTTAAATTTAGTCTCTTATGCCAACTCTGAGAGGCTTTCATGGCTTGAGTAGTGAGTGGAGTCTTGATTCGGGTATTTGGTACATATTGGGGAGATCGATGTCCTCTCTACGAGGTCTGCTCCCGTCTTGACTTGGAGTCCAGTTTCGTAAATTAGGGCGACCGCCTCTCTGCGTTTACAACTTGCCCTCGCCTGGTGATGATTGCGGAGACGTCTCTAATGTCATGTTTCTTGTGTTAAGTCATATAACTAGGGCAACTATTCCCGAATTACTACCTTGATGCGTGCAGTTTTAAACGGACGACAAACTTCTGCTATCGCTCGCCATAGCTATAGTCATCGCTTAAGATATTAACTCCAACCCGGCGGGCTGATAAAAGAATTCTCAGGTCTAAGCCCGCCGATCCATGACCGTCCCGAATGTGGGACCCCAGGGAGACCCGTTCGCCATGTAGGTAGACCCTCACTTCCGAACATCATCGCTGACTGCGGAAAAATAATTCTTCCCAGATATTAGAGCATTTCCGACATAGGTGATAT"
|> greaterThan1000OrPass
|> stringToDNAComplement
|> List.ofSeq
|> List.fold (fun acc item -> item::acc) []
|> List.iter (printf "%c")