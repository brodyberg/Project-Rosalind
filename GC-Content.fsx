//>Rosalind_6404
//CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC
//TCCCACTAATAATTCTGAGG
//>Rosalind_5959
//CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT
//ATATCCATTTGTCAGCAGACACGC
//>Rosalind_0808
//CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC
//TGGGAACCTGCGGGCAGTAGGTGGAAT

// this is a MAX() 
// read through list of FASTA format strings and store
// max as name and content percentage


// so roll through that list and make FASTA records

let isCharThis (goal:char) (item:char) = if item = goal then true else false
let isCharChevron item = isCharThis '>' item
let isCharNewline item = isCharThis '\n' item

let fastaName (items:seq<char>) = 
    items
    |> Seq.fold (fun acc item -> item::acc) ""


"".Split(['\n'])   

// we need to fold into a list of FASTA entries
// we can't map because we can't guarantee any fasta 
// entry will be simply one or two lines only
// but could be any number of lines

let (|FastaName|_|) (str:string) = None

let testItem = ">Rosalind_6404"

match testItem with
| FastaName(name) -> { Name = name; DNA = System.String.Empty; }
| _ -> { Name = "AddingToPrevious"; DNA = testItem; }
    

type Fasta = { Name:string; DNA:string; }

let fastaFromString (str:string) = { Name = "foo"; DNA = "acgt"; }

">Rosalind_6404
CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC
TCCCACTAATAATTCTGAGG
>Rosalind_5959
CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT
ATATCCATTTGTCAGCAGACACGC
>Rosalind_0808
CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC
TGGGAACCTGCGGGCAGTAGGTGGAAT".Split([|'\n'|])
// state machine: 
// if this line is a name, we make a new fasta
// if this line is data, we add this data to the last fasta dna
|> Seq.fold (fun acc item -> (fastaFromString item)::acc)
    []



|> Seq.iter (fun item -> printfn "line: %s" item)


|> Seq.iter (fun item -> 
printfn "%c" item) 



let lineStartsWith (item:char) = ""



// column max is 80, so we only make a new set info on >

// the GC-content of "AGCTATAG" is 37.5%
