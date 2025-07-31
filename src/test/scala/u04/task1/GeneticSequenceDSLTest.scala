package scala.u04.task1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class GeneticSequenceDSLTest extends AnyFlatSpec with should.Matchers:

  import nucleotide.{DNANucleotide, RNANucleotide}
  import GeneticSequence.*
  import GeneticSequenceDSL.{*, given}

  "GeneticSequenceDSL" should "create a DNA sequence by concatenating together the various DNANucleotide" in :
    val dnaSeq = DNANucleotide.A + DNANucleotide.T + DNANucleotide.C + DNANucleotide.G
    dnaSeq shouldEqual dna(List(DNANucleotide.A, DNANucleotide.T, DNANucleotide.C, DNANucleotide.G))

  it should "create an RNA sequence by concatenating together the various RNANucleotide" in :
    val rnaSeq = RNANucleotide.A + RNANucleotide.U + RNANucleotide.C + RNANucleotide.G
    rnaSeq shouldEqual rna(List(RNANucleotide.A, RNANucleotide.U, RNANucleotide.C, RNANucleotide.G))

  it should "not compile when trying to add two different type of Nucleotide" in :
      "val invalidSeq = DNANucleotide.A + RNANucleotide.A" shouldNot compile

  it should "not compile when trying to add a Nucleotide to a non-genetic sequence" in :
      "val invalidSeq = DNANucleotide.A + 42" shouldNot compile

  it should "add a DNANucleotide to a DNA sequence" in :
    val dnaSeq = dna(List(DNANucleotide.A, DNANucleotide.T))
    val newDnaSeq = dnaSeq + DNANucleotide.C
    newDnaSeq shouldEqual dna(List(DNANucleotide.A, DNANucleotide.T, DNANucleotide.C))

  it should "add a RNANucleotide to an RNA sequence" in :
    val rnaSeq = rna(List(RNANucleotide.A, RNANucleotide.U, RNANucleotide.C))
    val newRnaSeq = rnaSeq + RNANucleotide.G
    newRnaSeq shouldEqual rna(List(RNANucleotide.A, RNANucleotide.U, RNANucleotide.C, RNANucleotide.G))

  it should "chain two DNA sequences together" in :
    val seq1 = dna(List(DNANucleotide.A, DNANucleotide.T))
    val seq2 = dna(List(DNANucleotide.C, DNANucleotide.G))
    val chainedSeq = seq1 ++ seq2
    chainedSeq shouldEqual dna(List(DNANucleotide.A, DNANucleotide.T, DNANucleotide.C, DNANucleotide.G))

  it should "chain two RNA sequences together" in :
    val rnaSeq1 = rna(List(RNANucleotide.A, RNANucleotide.U))
    val rnaSeq2 = rna(List(RNANucleotide.C, RNANucleotide.G))
    val chainedRnaSeq = rnaSeq1 ++ rnaSeq2
    chainedRnaSeq shouldEqual rna(List(RNANucleotide.A, RNANucleotide.U, RNANucleotide.C, RNANucleotide.G))



