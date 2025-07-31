package scala.u04.task1.nucleotide

/**
 * Represents nucleotide with a method to get its complement.
 */
trait Nucleotide:
  def complement: Nucleotide

/**
 * DNA Nucleotide
 */
enum DNANucleotide extends Nucleotide:
  case A, T, C, G

  def complement: DNANucleotide = this match
    case A => T
    case T => A
    case C => G
    case G => C

  def toRna: RNANucleotide = this match
    case A => RNANucleotide.A
    case T => RNANucleotide.U
    case C => RNANucleotide.C
    case G => RNANucleotide.G

/**
 * RNA Nucleotide
 */
enum RNANucleotide extends Nucleotide:
  case A, U, C, G

  def complement: RNANucleotide = this match
    case A => U
    case U => A
    case C => G
    case G => C
