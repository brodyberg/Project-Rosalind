var DNA_ =   "GGGCTGATATAGAACGCGTCCATAAAGTCCATCGCGGCCCTAGGTGTTTTATCCCAAGCGCGTGTTCACTTGTAAAGGTGGAGGCTCTTGGCCGAAAGGTAAGTGCGAGTGGTAATGTGATGTTGGGTATTACAGGGTATTGACTCTATCTATCCGCATCGTTCGCTGGACAAACGCCCGCTTAACTTCACAAGTCTGATTACAATCCAAGCTGCGGATATCCAGGAGGAGTGTAGTATGCCTGTAGGACGGTGTCCGGGCGGGTTTCTAACTGATACTTCATAGGAAATCACTAGTGATATCGTGTTGCGGGATAGATTTACGGCCTATTGGCTCTCGATCGCTGAAGACACGCTGCCTCCAAATCATGGGCTGCGTACTATGGACCCCCGACGTTTGTAGTACCAGGAATCTTTATCTACTAATAACAGCTAAGCGTCGGTTCGTATATTTTGCCGATGGGCGCATGAGCCCGTGCTGTGTATAAATGACGCCGTAAGAGAGAGCACCGTGCGAAAGGGGGGGCCGTTATACACCTGATGTTGGCTGATATGCTTGCGTTAGTCTTGGATGAGTCTACGGTGTTTACGAAGCTAATTAGCAGCAGGCATGTTGTAAAACACTGCATTCCACTACATAGACGCACGCGGTACGAATTAGCAATGCTGATTCTGCCCCACGTGCTGCCCCCAAACGCTGCCTTTCTTGCCCCGAAGTAGCTCAATTACTACTCTATATGTCAGCCTGCCGAGCTGTTTGCACGTTATCTACTGACCACGCAAATACGGTCGCGAGCTTGGCTTATCCTTACCCACGTGGGTAGGGCATCGCGGAGCTGTGGACTCCTCTTGGGAAGCACACACCCCTTTAACAGGCTTATAGAAGTAC";

var DNA = "GATGGAACTTGACTACGTAAATT";
var outputString = "";

for (var i = 0; i < DNA.length; i++) {
    if (DNA[i] === "T") {
        outputString += "U";        
    }
    else {
        outputString += DNA[i];
    }
}

console.log(outputString);