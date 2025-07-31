package scala.u04.task1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.matchers.should.Matchers

class GeneticSequenceTest extends AnyFlatSpec with should.Matchers:
  import scala.u04.task1.GeneticSequence.*
  import scala.u04.task1.nucleotide.{DNANucleotide, RNANucleotide}

  "GeneticSequence" should "create a sequence from a list of elements" in :
    val dnaSeq = dna(List(DNANucleotide.A, DNANucleotide.T, DNANucleotide.C))
    dnaSeq.elements shouldEqual List(DNANucleotide.A, DNANucleotide.T, DNANucleotide.C)

    val rnaSeq = rna(List(RNANucleotide.A, RNANucleotide.U, RNANucleotide.C))
    rnaSeq.elements shouldEqual List(RNANucleotide.A, RNANucleotide.U, RNANucleotide.C)

  it should "create an empty sequence" in :
    val dnaSeq = dna(List())
    dnaSeq.elements shouldEqual List()

    val rnaSeq = rna(List())
    rnaSeq.elements shouldEqual List()

  it should "return the length of the sequence" in :
    val dnaSeq = dna(List(DNANucleotide.A, DNANucleotide.T, DNANucleotide.C))
    dnaSeq.length shouldEqual 3

    val rnaSeq = rna(List(RNANucleotide.A, RNANucleotide.U, RNANucleotide.C))
    rnaSeq.length shouldEqual 3

  it should "add an element to the sequence" in :
    val dnaSeq = dna(List(DNANucleotide.A, DNANucleotide.T))
    val newSeq = dnaSeq.add(DNANucleotide.C)
    newSeq.elements shouldEqual List(DNANucleotide.A, DNANucleotide.T, DNANucleotide.C)

    val rnaSeq = rna(List(RNANucleotide.A, RNANucleotide.U))
    val newRnaSeq = rnaSeq.add(RNANucleotide.G)
    newRnaSeq.elements shouldEqual List(RNANucleotide.A, RNANucleotide.U, RNANucleotide.G)
  
  it should "chain two sequences together" in :
    val seq1 = dna(List(DNANucleotide.A, DNANucleotide.T))
    val seq2 = dna(List(DNANucleotide.C, DNANucleotide.G))
    val chainedSeq = seq1.chain(seq2)
    chainedSeq.elements shouldEqual List(DNANucleotide.A, DNANucleotide.T, DNANucleotide.C, DNANucleotide.G)

    val rnaSeq1 = rna(List(RNANucleotide.A, RNANucleotide.U))
    val rnaSeq2 = rna(List(RNANucleotide.C, RNANucleotide.G))
    val chainedRnaSeq = rnaSeq1.chain(rnaSeq2)
    chainedRnaSeq.elements shouldEqual List(RNANucleotide.A, RNANucleotide.U, RNANucleotide.C, RNANucleotide.G)

  it should "check if it contains another sequence" in :
    val seq1 = dna(List(DNANucleotide.A, DNANucleotide.T))
    val seq2 = dna(List(DNANucleotide.A))
    val seq3 = dna(List(DNANucleotide.C))

    seq1.contains(seq2) shouldBe true
    seq1.contains(seq3) shouldBe false

    val rnaSeq1 = rna(List(RNANucleotide.A, RNANucleotide.U))
    val rnaSeq2 = rna(List(RNANucleotide.A))
    val rnaSeq3 = rna(List(RNANucleotide.C))
    rnaSeq1.contains(rnaSeq2) shouldBe true
    rnaSeq1.contains(rnaSeq3) shouldBe false

