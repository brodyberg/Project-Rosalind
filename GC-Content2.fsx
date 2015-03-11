module Fasta = 
    type Fasta = 
        { Name:string; DNA:string; GC_Content:double; }
        static member print (fasta:Fasta) = printfn "%s" (sprintf "%s\n%f" fasta.Name fasta.GC_Content)

    let strToFastaSeq (str:string) = 

        let split (splitter:char []) (str:string) = str.Split(splitter)
        let splitOnNewLine (str:string) = split [|'\n'|] str
        let splitOnChevron (str:string) = split [|'>'|] str
        let hasContent (str:string) = if str = System.String.Empty then false else true
            
        let emptyFasta = { Name = System.String.Empty; 
                            DNA = System.String.Empty; 
                            GC_Content = 0.0; }
        
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

">Rosalind_8327
GTTGAAATTACAATTGTCTCGACGTTCTAAATTAGATTACTTCAGTGTACTGCGCCTATT
>Rosalind_4695
GCGAAGGGTTGCAACCATTGTGCTACTACGGTGGCCGAAGCGAATCCATGCCAGTGGAGG"
|> Fasta.strToFastaSeq

