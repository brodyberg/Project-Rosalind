Seq.zip "GAGCCTACTAACGGGAT" "CATCGTAATGACGGCCT"
|> Seq.fold (fun acc pair -> if not (fst pair = snd pair) then acc + 1 else acc) 0