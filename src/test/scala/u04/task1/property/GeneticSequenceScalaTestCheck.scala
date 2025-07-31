package scala.u04.task1.property

import org.scalatest.*
import matchers.*
import org.scalatest.propspec.AnyPropSpec
import prop.*

import scala.collection.immutable.*
import scala.u04.task1.nucleotide.{DNANucleotide, RNANucleotide}
import scala.u04.task1.GeneticSequence
import scala.u04.task1.GeneticSequence.{DNA, RNA}

object GeneticSequenceScalaTestCheck extends AnyPropSpec with TableDrivenPropertyChecks with should.Matchers:

  val dnaSequenceExamples: TableFor3[String, DNA, List[DNANucleotide]] =
    Table(
      ("description", "sequence", "elements"),
      ("empty DNA", GeneticSequence.dna(List.empty), List.empty),
      ("single DNA", GeneticSequence.dna(List(DNANucleotide.A)), List(DNANucleotide.A)),
      ("multiple DNA", GeneticSequence.dna(List(DNANucleotide.A, DNANucleotide.T, DNANucleotide.G)),
        List(DNANucleotide.A, DNANucleotide.T, DNANucleotide.G))
    )

  val rnaSequenceExamples: TableFor3[String, RNA, List[RNANucleotide]] =
    Table(
      ("description", "sequence", "elements"),
      ("empty RNA", GeneticSequence.rna(List.empty), List.empty),
      ("single RNA", GeneticSequence.rna(List(RNANucleotide.U)), List(RNANucleotide.U)),
      ("multiple RNA", GeneticSequence.rna(List(RNANucleotide.A, RNANucleotide.U, RNANucleotide.G)),
        List(RNANucleotide.A, RNANucleotide.U, RNANucleotide.G))
    )

  val dnaAddExamples: TableFor4[String, List[DNANucleotide], DNANucleotide, List[DNANucleotide]] =
    Table(
      ("description", "originalList", "elementToAdd", "expectedList"),
      ("add to empty", List.empty[DNANucleotide], DNANucleotide.A, List(DNANucleotide.A)),
      ("add to single", List(DNANucleotide.T), DNANucleotide.G, List(DNANucleotide.T, DNANucleotide.G)),
      ("add to multiple", List(DNANucleotide.A, DNANucleotide.T), DNANucleotide.C,
        List(DNANucleotide.A, DNANucleotide.T, DNANucleotide.C))
    )

  val dnaChainExamples: TableFor4[String, List[DNANucleotide], List[DNANucleotide], List[DNANucleotide]] =
    Table(
      ("description", "list1", "list2", "expectedList"),
      ("chain two empty", List.empty[DNANucleotide], List.empty[DNANucleotide], List.empty[DNANucleotide]),
      ("chain empty + non-empty", List.empty[DNANucleotide], List(DNANucleotide.A), List(DNANucleotide.A)),
      ("chain non-empty + empty", List(DNANucleotide.T), List.empty[DNANucleotide], List(DNANucleotide.T)),
      ("chain two singles", List(DNANucleotide.A), List(DNANucleotide.T), List(DNANucleotide.A, DNANucleotide.T)),
      ("chain multiple", List(DNANucleotide.A, DNANucleotide.T), List(DNANucleotide.G, DNANucleotide.C),
        List(DNANucleotide.A, DNANucleotide.T, DNANucleotide.G, DNANucleotide.C))
    )

  property("DNA constructor axiom: elements(dna(xs)) should equal xs"):
    forAll(dnaSequenceExamples): (_, sequence, elements) =>
      sequence.elements should be (elements)
  
  property("RNA constructor axiom: elements(rna(xs)) should equal xs"):
    forAll(rnaSequenceExamples): (_, sequence, elements) =>
      sequence.elements should be (elements)
      
  property("DNA length axiom: length(dna(xs)) should equal xs.length"):
    forAll(dnaSequenceExamples): (_, sequence, elements) =>
      sequence.length should be (elements.length)
      
  property("RNA length axiom: length(rna(xs)) should equal xs.length"):
    forAll(rnaSequenceExamples):(_, sequence, elements) =>
      sequence.length should be (elements.length)
  
  property("DNA add axiom: add(x)(dna(xs)) should equal dna(xs :+ x)"):
    forAll(dnaAddExamples): (_, originalList, elementToAdd, expectedList) =>
      val originalSeq = GeneticSequence.dna(originalList)
      val resultSeq = originalSeq.add(elementToAdd)
      val expectedSeq = GeneticSequence.dna(expectedList)
      resultSeq.elements should be (expectedSeq.elements)
  
  property("DNA chain axiom: chain(dna(xs))(dna(ys)) should equal dna(xs ++ ys)"):
    forAll(dnaChainExamples): (_, list1, list2, expectedList) =>
      val seq1 = GeneticSequence.dna(list1)
      val seq2 = GeneticSequence.dna(list2)
      val resultSeq = seq1.chain(seq2)
      val expectedSeq = GeneticSequence.dna(expectedList)
      resultSeq.elements should be (expectedSeq.elements)
      
  property("DNA add operation should increase length by one"):
    forAll(dnaAddExamples): (_, originalList, elementToAdd, _) =>
      val originalSeq = GeneticSequence.dna(originalList)
      val resultSeq = originalSeq.add(elementToAdd)
      resultSeq.length should be (originalSeq.length + 1)
  
  property("DNA chain operation should have additive length"):
    forAll(dnaChainExamples): (_, list1, list2, _) =>
      val seq1 = GeneticSequence.dna(list1)
      val seq2 = GeneticSequence.dna(list2)
      val resultSeq = seq1.chain(seq2)
      resultSeq.length should be (seq1.length + seq2.length)
  
  property("DNA chain left identity: empty chain seq should equal seq"):
    val nonEmptyExamples = dnaSequenceExamples.filter(_._3.nonEmpty)
    forAll(nonEmptyExamples): (_, sequence, _) =>
      val emptySeq = GeneticSequence.dna(List.empty)
      val result = emptySeq.chain(sequence)
      result.elements should be (sequence.elements)
  
  property("DNA chain right identity: seq chain empty should equal seq"):
    forAll(dnaSequenceExamples): (_, sequence, _) =>
      val emptySeq = GeneticSequence.dna(List.empty)
      val result = sequence.chain(emptySeq)
      result.elements should be (sequence.elements)
  
  property("DNA add should be equivalent to chain with singleton"):
    forAll(dnaAddExamples): (_, originalList, elementToAdd, _) =>
      val originalSeq = GeneticSequence.dna(originalList)
      val singletonSeq = GeneticSequence.dna(List(elementToAdd))
      val addResult = originalSeq.add(elementToAdd)
      val chainResult = originalSeq.chain(singletonSeq)
      addResult.elements should be (chainResult.elements)
    
  
