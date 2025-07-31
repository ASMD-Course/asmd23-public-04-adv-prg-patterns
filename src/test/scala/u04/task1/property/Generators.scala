package scala.u04.task1.property

import org.scalacheck.{Arbitrary, Gen}

import scala.u04.task1.GeneticSequence
import scala.u04.task1.nucleotide.{DNANucleotide, RNANucleotide}

/**
 * Provides generators for DNA and RNA sequences and their nucleotides.
 * This is used for property-based testing.
 */
object Generators :
  /**
   * Generates a list of DNA nucleotides.
   * The list can be empty or contain a random sequence of nucleotides.
   */
  private def dnaListGen(): Gen[List[DNANucleotide]] = for
    nucleotide <- Gen.oneOf(DNANucleotide.A, DNANucleotide.T, DNANucleotide.G, DNANucleotide.C)
    continue <- Gen.prob(0.7)
    rest <- if continue then dnaListGen() else Gen.const(List.empty[DNANucleotide])
  yield nucleotide :: rest
  
  /**
   * Generates a list of RNA nucleotides.
   * The list can be empty or contain a random sequence of nucleotides.
   */
  private def rnaListGen(): Gen[List[RNANucleotide]] = for
    nucleotide <- Gen.oneOf(RNANucleotide.A, RNANucleotide.U, RNANucleotide.G, RNANucleotide.C)
    continue <- Gen.prob(0.7)
    rest <- if continue then rnaListGen() else Gen.const(List.empty[RNANucleotide])
  yield nucleotide :: rest

  /**
   * Generates a DNA sequence from a list of DNA nucleotides.
   * The sequence is wrapped in a GeneticSequence object.
   */
  private def dnaSequenceGen(): Gen[GeneticSequence[DNANucleotide]] = dnaListGen().map(GeneticSequence.dna)
  
  /**
   * Generates an RNA sequence from a list of RNA nucleotides.
   * The sequence is wrapped in a GeneticSequence object.
   */
  private def rnaSequenceGen(): Gen[GeneticSequence[RNANucleotide]] = rnaListGen().map(GeneticSequence.rna)

  given dnaListArbitrary: Arbitrary[List[DNANucleotide]] = Arbitrary(dnaListGen())
  given rnaListArbitrary: Arbitrary[List[RNANucleotide]] = Arbitrary(rnaListGen())

  given dnaSequenceArbitrary: Arbitrary[GeneticSequence[DNANucleotide]] = Arbitrary(dnaSequenceGen())
  given rnaSequenceArbitrary: Arbitrary[GeneticSequence[RNANucleotide]] = Arbitrary(rnaSequenceGen())
  
  given dnaNucleotideArbitrary: Arbitrary[DNANucleotide] = Arbitrary:
    Gen.oneOf(DNANucleotide.A, DNANucleotide.T, DNANucleotide.G, DNANucleotide.C)
  
  given rnaNucleotideArbitrary: Arbitrary[RNANucleotide] = Arbitrary:
    Gen.oneOf(RNANucleotide.A, RNANucleotide.U, RNANucleotide.G, RNANucleotide.C)
  
  @main def showGeneticSequences() =
    import Generators.{*, given}
    
    Range(0, 10).foreach: i =>
      val sample = summon[Arbitrary[GeneticSequence[DNANucleotide]]].arbitrary.sample
      println(s"Sample $i: ${sample.map(_.elements).getOrElse("None")}")

    Range(0, 10).foreach: i =>
      val sample = summon[Arbitrary[GeneticSequence[RNANucleotide]]].arbitrary.sample
      println(s"Sample $i: ${sample.map(_.elements).getOrElse("None")}")

    Range(0, 10).foreach: i =>
      val sample = summon[Arbitrary[List[DNANucleotide]]].arbitrary.sample
      println(s"Sample $i: ${sample.getOrElse("None")}")

