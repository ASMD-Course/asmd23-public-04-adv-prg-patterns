package scala.u04.task1.property

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

import scala.u04.task1.GeneticSequence
import scala.u04.task1.nucleotide.{DNANucleotide, RNANucleotide}
import org.scalacheck.Test.Parameters

object GeneticSequenceCheck extends Properties("GeneticSequence"):
  import Generators.{*, given}

  // Here we override the default parameters for the property tests
  override def overrideParameters(p: Parameters): Parameters = p.withMinSuccessfulTests(500)
    .withMaxDiscardRatio(10.0f)   // Not necessary, only tested for the task purpose
    .withMaxSize(50)              // size of the generated sequences


  property("dnaConstructorAxiom") =
    forAll: (xs: List[DNANucleotide]) =>
      GeneticSequence.dna(xs).elements == xs
  property("rnaConstructorAxiom") =
    forAll: (xs: List[RNANucleotide]) =>
      GeneticSequence.rna(xs).elements == xs

  property("emptyDnaAxiom") =
    GeneticSequence.dna(List.empty).elements == List.empty
  property("emptyRnaAxiom") =
    GeneticSequence.rna(List.empty).elements == List.empty

  property("dnaLengthAxiom") =
    forAll: (xs: List[DNANucleotide]) =>
      GeneticSequence.dna(xs).length == xs.length
  property("rnaLengthAxiom") =
    forAll: (xs: List[RNANucleotide]) =>
      GeneticSequence.rna(xs).length == xs.length

  property("dnaAddAxiom") =
    forAll: (xs: List[DNANucleotide], x: DNANucleotide) =>
      val seq = GeneticSequence.dna(xs)
      val expected = GeneticSequence.dna(xs :+ x)
      seq.add(x).elements == expected.elements

  property("rnaAddAxiom") =
    forAll: (xs: List[RNANucleotide], x: RNANucleotide) =>
      val seq = GeneticSequence.rna(xs)
      val expected = GeneticSequence.rna(xs :+ x)
      seq.add(x).elements == expected.elements

  property("dnaChainAxiom") =
    forAll: (xs: List[DNANucleotide], ys: List[DNANucleotide]) =>
      val seq1 = GeneticSequence.dna(xs)
      val seq2 = GeneticSequence.dna(ys)
      val expected = GeneticSequence.dna(xs ++ ys)
      seq1.chain(seq2).elements == expected.elements

  property("rnaChainAxiom") =
    forAll: (xs: List[RNANucleotide], ys: List[RNANucleotide]) =>
      val seq1 = GeneticSequence.rna(xs)
      val seq2 = GeneticSequence.rna(ys)
      val expected = GeneticSequence.rna(xs ++ ys)
      seq1.chain(seq2).elements == expected.elements

  property("addLengthConsistency") =
    forAll: (seq: GeneticSequence[DNANucleotide], x: DNANucleotide) =>
      seq.add(x).length == seq.length + 1
  property("rnaAddLengthConsistency") =
    forAll: (seq: GeneticSequence[RNANucleotide], x: RNANucleotide) =>
      seq.add(x).length == seq.length + 1

  property("dnaChainLengthAdditivity") =
    forAll: (seq1: GeneticSequence[DNANucleotide], seq2: GeneticSequence[DNANucleotide]) =>
      seq1.chain(seq2).length == seq1.length + seq2.length
  property("rnaChainLengthAdditivity") =
    forAll: (seq1: GeneticSequence[RNANucleotide], seq2: GeneticSequence[RNANucleotide]) =>
      seq1.chain(seq2).length == seq1.length + seq2.length

  property("dnaChainAssociativity") =
    forAll: (xs: List[DNANucleotide], ys: List[DNANucleotide], zs: List[DNANucleotide]) =>
      val seq1 = GeneticSequence.dna(xs)
      val seq2 = GeneticSequence.dna(ys)
      val seq3 = GeneticSequence.dna(zs)
      val leftAssoc = seq1.chain(seq2).chain(seq3)
      val rightAssoc = seq1.chain(seq2.chain(seq3))
      leftAssoc.elements == rightAssoc.elements

  property("rnaChainAssociativity") =
    forAll: (xs: List[RNANucleotide], ys: List[RNANucleotide], zs: List[RNANucleotide]) =>
      val seq1 = GeneticSequence.rna(xs)
      val seq2 = GeneticSequence.rna(ys)
      val seq3 = GeneticSequence.rna(zs)
      val leftAssoc = seq1.chain(seq2).chain(seq3)
      val rightAssoc = seq1.chain(seq2.chain(seq3))
      leftAssoc.elements == rightAssoc.elements

  property("dnaChainIdentity") =
    forAll: (xs: List[DNANucleotide]) =>
      val seq = GeneticSequence.dna(xs)
      val empty = GeneticSequence.dna(List.empty)
      (seq.chain(empty).elements == seq.elements) &&
        (empty.chain(seq).elements == seq.elements)

  property("rnaChainIdentity") =
    forAll: (xs: List[RNANucleotide]) =>
      val seq = GeneticSequence.rna(xs)
      val empty = GeneticSequence.rna(List.empty)
      (seq.chain(empty).elements == seq.elements) &&
        (empty.chain(seq).elements == seq.elements)

  property("dnaAddChainRelationship") =
    forAll: (xs: List[DNANucleotide], x: DNANucleotide) =>
      val seq = GeneticSequence.dna(xs)
      val singleton = GeneticSequence.dna(List(x))
      seq.add(x).elements == seq.chain(singleton).elements

  property("rnaAddChainRelationship") =
    forAll: (xs: List[RNANucleotide], x: RNANucleotide) =>
      val seq = GeneticSequence.rna(xs)
      val singleton = GeneticSequence.rna(List(x))
      seq.add(x).elements == seq.chain(singleton).elements

  property("patternMatchingAxioms") =
    forAll: (xs: List[DNANucleotide], x: DNANucleotide) =>
      xs match
        case Nil =>
          val emptySeq = GeneticSequence.dna(Nil)
          emptySeq.add(x).elements == List(x)
        case _ =>
          val seq = GeneticSequence.dna(xs)
          seq.add(x).elements == (xs :+ x)

