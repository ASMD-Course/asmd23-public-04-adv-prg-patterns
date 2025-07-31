package scala.u04.task1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.u04.task1.nucleotide.{DNANucleotide, RNANucleotide}

class NucleotideTest extends AnyFlatSpec with should.Matchers:
  
  "DNANucleotide" should "return the correct complement" in:
    DNANucleotide.A.complement shouldEqual DNANucleotide.T
    DNANucleotide.T.complement shouldEqual DNANucleotide.A
    DNANucleotide.C.complement shouldEqual DNANucleotide.G
    DNANucleotide.G.complement shouldEqual DNANucleotide.C
  
  "RNANucleotide" should "return the correct complement" in:
    RNANucleotide.A.complement shouldEqual RNANucleotide.U
    RNANucleotide.U.complement shouldEqual RNANucleotide.A
    RNANucleotide.C.complement shouldEqual RNANucleotide.G
    RNANucleotide.G.complement shouldEqual RNANucleotide.C
  
  "DNANucleotide" should "convert to RNA correctly" in:
    DNANucleotide.A.toRna shouldEqual RNANucleotide.A
    DNANucleotide.T.toRna shouldEqual RNANucleotide.U
    DNANucleotide.C.toRna shouldEqual RNANucleotide.C
    DNANucleotide.G.toRna shouldEqual RNANucleotide.G
  