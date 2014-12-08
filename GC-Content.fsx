[<RequireQualifiedAccess>]
module Fasta = 
    
    type Fasta = 
        { Name:string; DNA:string; }
        static member print (fasta:Fasta) = printfn "Name: %s DNA: %s" fasta.Name fasta.DNA
    
    let strToFastaSeq (str:string) = 

        let split (splitter:char []) (str:string) = str.Split(splitter)
        let splitOnNewLine (str:string) = split [|'\n'|] str
        let splitOnChevron (str:string) = split [|'>'|] str
        
        let emptyFasta = { Name = System.String.Empty; 
                           DNA = System.String.Empty; }
        
        let entryToFasta (items:string []) =
            items
            |> Seq.fold (fun acc item -> 
                match acc.Name with
                | "" -> { acc with Name = item; }
                | _ -> { acc with DNA = acc.DNA + item.Trim(); })
                emptyFasta
        
        let hasContent (str:string) = if str = System.String.Empty then false else true
    
        str
        |> splitOnChevron
        |> Seq.filter hasContent
        |> Seq.map splitOnNewLine
        |> Seq.map (fun item -> entryToFasta item)
    
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














