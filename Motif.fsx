//s: GATATATGCATATACTT
//t: ATAT

//output: 2 4 10

// we take s, we take t
// we iterate one char at a time through s
// match next n (n = length of t) in s against t

let s = "GATATATGCATATACTT"
let t = "ATAT"

"GATATATGCATATACTT"
|> Seq.iteri (fun i item -> 
    printfn "i: %d" i

    if item = 'A' then
        if (i < 10) then
            let sub = s.Substring(i, t.Length)
            printfn "sub: %s" sub
        
            if sub = t then printfn "MATCH: %d" i)

// NOTE: answer is 1-based, not zero-based