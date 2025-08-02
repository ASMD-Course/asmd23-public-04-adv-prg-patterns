# Task 1

For the first task I've defined a basic model of genetic sequences using Scala. 
The model includes two types of nucleotides: DNA and RNA, each represented by a case class. The `GeneticSequence` type is 
defined as a generic type that can hold either DNA or RNA nucleotides.

```text
type :
    GeneticSequence[A <: Nucleotide] =
    
constructors :
    dna: List[DNANucleotide] => GeneticSequence[DNANucleotide]
    rna: List[RNANucleotide] => GeneticSequence[RNANucleotide]

operations :
    elements: List[A]
    length: Int
    add: A => GeneticSequence[A]
    chain: GeneticSequence[A] => GeneticSequence[A]

axioms :
    elements(dna(xs)) = xs
    elements(rna(xs)) = xs
    elements(rna()) = E
    elements(dna()) = E
    
    length(dna(xs)) = xs.length
    length(rna(xs)) = xs.length
    
    add(x: Dn)(dna(xs)) = dna(xs :+ x)
    add(x)(rna(xs)) = rna(xs :+ x)
   
    chain(dna(xs))(dna(ys)) = dna(xs ++ ys)
    chain(rna(xs))(rna(ys)) = rna(xs ++ ys)>
```