//s: GATATATGCATATACTT
//t: ATAT


//output: 2 4 10

// we take s, we take t
// we iterate one char at a time through s
// match next n (n = length of t) in s against t

let s = "GATATATGCATATACTT"
let t = "ATAT"
//let sub = t.Substring(0, (0 + (t.Length)))

"GATATATGCATATACTT"
|> Seq.iteri (fun i item -> 
    printfn "i: %d" i
    match item with
    | 'A' -> 
//        if (i < s.Length) && (i + t.Length) < s.Length then
        if (i < 10) then
            let sub = s.Substring(i, (i + (t.Length)))
            printf "sub: %s" sub
        
            if sub = t then printf " %d" i
    | _ -> printf "_")


"GATATATGCATATACTT"
|> Seq.take 5



Seq.compareWith

// NOTE: answer is 1-based, not zero-based

let findTinS (t:string) (s:string) =
    s
    |> Seq.findIndex (fun 