[<RequireQualifiedAccess>]
module Fasta = 
    type internal Content = { C_or_G:double; Other:double; }

    type Fasta = 
        { Name:string; DNA:string; }
        static member print (fasta:Fasta) = printfn "Name: %s DNA: %s" fasta.Name fasta.DNA
        static member GC_Content (fasta:Fasta) = 
            let emptyContent = { C_or_G = 0.0; Other = 0.0; }
        
            let calculate content = 
                (content.C_or_G / (content.C_or_G + content.Other)) * 100.0

            fasta.DNA
            |> Seq.fold (fun acc item ->
                match item with
                | 'C' | 'G' -> { acc with Content.C_or_G = acc.C_or_G + 1.0; }
                | _ -> { acc with Content.Other = acc.Other + 1.0; })
                emptyContent
            |> calculate
    
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

let printCalculation calculation = 
    printfn "%f%%" calculation

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
|> Seq.map (fun fasta -> Fasta.Fasta.GC_Content fasta)
|> Seq.iter (fun gc -> printCalculation gc)

">Rosalind_6404
AGCTATAG"
// GC content is percentage of the symbols that are C or G
// GC-content of "AGCTATAG" is 37.5%













