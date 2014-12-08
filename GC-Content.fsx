[<RequireQualifiedAccess>]
module Fasta = 
    
    type Fasta = 
        { Name:string; DNA:string; }
        static member print (fasta:Fasta) = printfn "Name: %s DNA: %s" fasta.Name fasta.DNA
    
    let strToFastaSeq (str:string) = 

        let split (splitter:char []) (str:string) = str.Split(splitter)
        let splitOnNewLine (str:string) = split [|'\n'|] str
        let splitOnChevron (str:string) = split [|'>'|] str
        let hasContent (str:string) = if str = System.String.Empty then false else true
            
        let emptyFasta = { Name = System.String.Empty; 
                           DNA = System.String.Empty; }
        
        let entryToFasta (lines:string []) =
            lines
            |> Seq.fold (fun fasta line -> 
                match fasta.Name with
                | "" -> { fasta with Name = line; }
                | _ -> { fasta with DNA = fasta.DNA + line.Trim(); })
                emptyFasta
        
        str
        |> splitOnChevron
        |> Seq.filter hasContent
        |> Seq.map splitOnNewLine
        |> Seq.map entryToFasta
    
">Rosalind_6404
CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC
TCCCACTAATAATTCTGAGG
>Rosalind_5959
CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT
ATATCCATTTGTCAGCAGACACGC
>Rosalind_0808
CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC
TGGGAACCTGCGGGCAGTAGGTGGAAT"
|> Fasta.strToFastaSeq
|> Seq.iter Fasta.Fasta.print














