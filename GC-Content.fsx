type Fasta = 
    { Name:string; DNA:string; }
    static member print (fasta:Fasta) = printfn "Name: %s DNA: %s" fasta.Name fasta.DNA

//Fasta.print { Name = "foo"; DNA = "bar"; }

//let isCharThis (goal:char) (item:char) = if item = goal then true else false
//let isCharChevron item = isCharThis '>' item
//let isCharNewline item = isCharThis '\n' item

let split (splitter:char []) (str:string) = str.Split(splitter)
let splitOnNewLine (str:string) = split [|'\n'|] str
let splitOnChevron (str:string) = split [|'>'|] str

let emptyFasta = { Name = System.String.Empty; 
                   DNA = System.String.Empty; }

//let (|FastaName|_|) (str:string) = 
////    match hasContent str with
////    | true -> 
//    if isCharChevron (str.[0]) 
//    then Some(str.Substring(1, (str.Length - 1)))
//    else None
////    | false -> None

let entryToFasta (items:string []) =
    items
    |> Seq.fold (fun acc item -> 
        match acc.Name with
        | "" -> { acc with Name = item; }
        | _ -> { acc with DNA = acc.DNA + item.Trim(); })
        emptyFasta

//let entryToFasta (items:string []) = 
//    items
//    |> Seq.fold (fun acc item -> 
//        match item with
//        | "" -> acc
//        | FastaName(name) -> { acc with Name = name; }
//        | _ -> { acc with DNA = acc.DNA + item; }) 
//        emptyFasta

let hasContent (str:string) = if str = System.String.Empty then false else true

//let fastaHasContent fasta = if fasta.Name = System.String.Empty then false else true

">Rosalind_6404
CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC
TCCCACTAATAATTCTGAGG
>Rosalind_5959
CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT
ATATCCATTTGTCAGCAGACACGC
>Rosalind_0808
CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC
TGGGAACCTGCGGGCAGTAGGTGGAAT"
|> splitOnChevron
|> Seq.filter hasContent
|> Seq.map splitOnNewLine
|> Seq.map (fun item -> entryToFasta item)
//|> Seq.filter fastaHasContent
|> Seq.iter (fun fasta -> Fasta.print fasta)















