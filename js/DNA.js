var DNA =   "GGGCTGATATAGAACGCGTCCATAAAGTCCATCGCGGCCCTAGGTGTTTTATCCCAAGCGCGTGTTCACTTGTAAAGGTGGAGGCTCTTGGCCGAAAGGTAAGTGCGAGTGGTAATGTGATGTTGGGTATTACAGGGTATTGACTCTATCTATCCGCATCGTTCGCTGGACAAACGCCCGCTTAACTTCACAAGTCTGATTACAATCCAAGCTGCGGATATCCAGGAGGAGTGTAGTATGCCTGTAGGACGGTGTCCGGGCGGGTTTCTAACTGATACTTCATAGGAAATCACTAGTGATATCGTGTTGCGGGATAGATTTACGGCCTATTGGCTCTCGATCGCTGAAGACACGCTGCCTCCAAATCATGGGCTGCGTACTATGGACCCCCGACGTTTGTAGTACCAGGAATCTTTATCTACTAATAACAGCTAAGCGTCGGTTCGTATATTTTGCCGATGGGCGCATGAGCCCGTGCTGTGTATAAATGACGCCGTAAGAGAGAGCACCGTGCGAAAGGGGGGGCCGTTATACACCTGATGTTGGCTGATATGCTTGCGTTAGTCTTGGATGAGTCTACGGTGTTTACGAAGCTAATTAGCAGCAGGCATGTTGTAAAACACTGCATTCCACTACATAGACGCACGCGGTACGAATTAGCAATGCTGATTCTGCCCCACGTGCTGCCCCCAAACGCTGCCTTTCTTGCCCCGAAGTAGCTCAATTACTACTCTATATGTCAGCCTGCCGAGCTGTTTGCACGTTATCTACTGACCACGCAAATACGGTCGCGAGCTTGGCTTATCCTTACCCACGTGGGTAGGGCATCGCGGAGCTGTGGACTCCTCTTGGGAAGCACACACCCCTTTAACAGGCTTATAGAAGTAC";

var counts = { A: 0,
               C: 0,
               G: 0,
               T: 0 };

// compute four integers:
// counts of a c g t

function countItems(dna, counts) {
    for (var i = 0; i < DNA.length; i++) {
        switch(DNA[i]) 
        {
            case 'A':
                counts.A += 1; 
                break;
                
            case 'C':
                counts.C += 1; 
                break;
            case 'G':
                counts.G += 1; 
                break;
            case 'T':
                counts.T += 1; 
                break;            
        }
    }
    
    return counts;     
}

var newCounts = countItems(DNA, counts);

var x; 
var outputString = ""; 
for (x in newCounts) {
    outputString += newCounts[x] + " ";    
}

// correct answer: 207 214 233 234

console.log(outputString);