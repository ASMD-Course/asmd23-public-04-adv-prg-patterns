package scala.u04.task1

import scala.language.postfixOps
import scala.u04.task1.nucleotide.{DNANucleotide, Nucleotide, RNANucleotide}

trait GeneticSequence[A <: Nucleotide]:
  /**
   * Returns the elements of the genetic sequence.
   */
  def elements: List[A]

  /**
   * Returns the length of the genetic sequence.
   */
  def length: Int

  /**
   * Adds an element to the genetic sequence.
   * @param element the element to add
   * @return a new genetic sequence with the added element
   */
  infix def add(element: A): GeneticSequence[A]

  /**
   * Chains two genetic sequences together.
   * @param other the other genetic sequence to chain
   * @return a new genetic sequence that is the concatenation of this and the other
   */
  infix def chain(other: GeneticSequence[A]): GeneticSequence[A]

  /**
   * Checks if the genetic sequence contains another genetic sequence.
   * @param geneticSequence the genetic sequence to check for
   * @return true if this genetic sequence contains the other, false otherwise
   */
  def contains(geneticSequence: GeneticSequence[A]): Boolean

object GeneticSequence:

  type DNA = GeneticSequence[DNANucleotide]
  type RNA = GeneticSequence[RNANucleotide]

  private case class GeneticSequenceImpl[A <: Nucleotide](elements: List[A]) extends GeneticSequence[A]:
    override def length: Int = elements.length
    override def add(element: A): GeneticSequence[A] = GeneticSequenceImpl(elements :+ element)
    override def chain(other: GeneticSequence[A]): GeneticSequence[A] = GeneticSequence(
      this.elements ++ other.elements
    )
    override def contains(geneticSequence: GeneticSequence[A]): Boolean =
      this.elements.containsSlice(geneticSequence.elements)

  /**
   * Creates a new genetic sequence from a list of elements.
   * @param elements the list of elements to create the genetic sequence from
   * @tparam A the type of elements in the genetic sequence
   * @return a new genetic sequence containing the elements
   */
  private def apply[A <: Nucleotide](elements: List[A]): GeneticSequence[A] = GeneticSequenceImpl(elements)

  /**
   * Creates a DNA genetic sequence from a list of DNA nucleotides.
   * @param elements the list of DNA nucleotides to create the genetic sequence from
   * @return
   */
  def dna(elements: List[DNANucleotide]): DNA = GeneticSequence(elements)
  
  /**
   * Creates an RNA genetic sequence from a list of RNA nucleotides.
   * @param elements the list of RNA nucleotides to create the genetic sequence from
   * @return
   */
  def rna(elements: List[RNANucleotide]): RNA = GeneticSequence(elements)


/**
 * A DSL for creating and manipulating genetic sequences.
 * It provides a way to create sequences of DNA and RNA nucleotides,
 * add elements to them, and chain sequences together.
 */
object GeneticSequenceDSL:
  import scala.language.implicitConversions

  trait SequenceFactory[A <: Nucleotide]:
    def createSequence(elements: List[A]): GeneticSequence[A]
    def single(element: A): GeneticSequence[A] = createSequence(List(element))
  
  given SequenceFactory[DNANucleotide] with
    def createSequence(elements: List[DNANucleotide]): GeneticSequence[DNANucleotide] = GeneticSequence.dna(elements)

  given SequenceFactory[RNANucleotide] with
    def createSequence(elements: List[RNANucleotide]): GeneticSequence[RNANucleotide] = GeneticSequence.rna(elements)

  given [A <: Nucleotide: SequenceFactory]: Conversion[A, GeneticSequence[A]] with
    def apply(nucleotide: A): GeneticSequence[A] = summon[SequenceFactory[A]].single(nucleotide)

  extension [A <: Nucleotide](geneticSequence: GeneticSequence[A])
    infix def +(element: A): GeneticSequence[A] = geneticSequence.add(element)
    infix def ++(other: GeneticSequence[A]): GeneticSequence[A] = geneticSequence.chain(other)

  extension [A <: Nucleotide: SequenceFactory](element: A)
    infix def +(other: A): GeneticSequence[A] = summon[SequenceFactory[A]].createSequence(List(element, other))
    infix def +(other: GeneticSequence[A]): GeneticSequence[A] = summon[SequenceFactory[A]].single(element) ++ other

@main
def tryGeneticSequence(): Unit =
  import GeneticSequenceDSL.{*, given}

  val newTest = DNANucleotide.G + DNANucleotide.T + DNANucleotide.A + DNANucleotide.C
  val newTest2 = RNANucleotide.A + RNANucleotide.U + RNANucleotide.C + RNANucleotide.G 


