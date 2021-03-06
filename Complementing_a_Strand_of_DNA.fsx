"GTTAGTTATGGTTGCAGTCCATCGTTAGTCCTCTTCTATTTGGTCACATCTCGCGTCTGTATCTCATACTATGTTCTTCTCGAAGGTAACTAGAGCGGTTTAGGAGAAGCTTTTGACGACGCTACTTCTTGGACGCTCCTTGGCGTCACCCCATGTAACCAACTCACTGACACTTATCGGCGCATTCTTTATGTTACCTGCGCCGAAGGGTACGTCTTCAGAACTTTAGCTTCCACAATCGCCTTGCCGATGTGACCCCTTCCACCCGTGATACCTGTGCGACTGACTTTTGGTCAGTCCTGAAGGACTCTACTGAATCATAAAACTTCCCGTTTTATGGATTGGTTATACTATAAGGCTGGTTCGCAGAACAACTAGCGTCTGTCACTTAATCCCCCCCATGTATACCGTTACCTAGCCCTCCTTAAATTTAGTCTCTTATGCCAACTCTGAGAGGCTTTCATGGCTTGAGTAGTGAGTGGAGTCTTGATTCGGGTATTTGGTACATATTGGGGAGATCGATGTCCTCTCTACGAGGTCTGCTCCCGTCTTGACTTGGAGTCCAGTTTCGTAAATTAGGGCGACCGCCTCTCTGCGTTTACAACTTGCCCTCGCCTGGTGATGATTGCGGAGACGTCTCTAATGTCATGTTTCTTGTGTTAAGTCATATAACTAGGGCAACTATTCCCGAATTACTACCTTGATGCGTGCAGTTTTAAACGGACGACAAACTTCTGCTATCGCTCGCCATAGCTATAGTCATCGCTTAAGATATTAACTCCAACCCGGCGGGCTGATAAAAGAATTCTCAGGTCTAAGCCCGCCGATCCATGACCGTCCCGAATGTGGGACCCCAGGGAGACCCGTTCGCCATGTAGGTAGACCCTCACTTCCGAACATCATCGCTGACTGCGGAAAAATAATTCTTCCCAGATATTAGAGCATTTCCGACATAGGTGATAT"
|> Seq.fold (fun acc item -> item::acc) []
|> Seq.iter (fun c -> 
    match c with 
    | 'A' -> printf "T" 
    | 'T' -> printf "A"
    | 'C' -> printf "G"
    | 'G' -> printf "C"
    | _ -> printf "%c" c)