package alu

import chisel3._
import chiseltest._
import chiseltest.formal._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._

import testutil._

class AluMiterWithChecker(factory0 : () => AluIfc, factory1 : () => AluIfc) extends AluMiter(factory0, factory1) {
  assert(same || (mode =/= 1.U && mode =/= 2.U && mode =/= 4.U))
}

class AluMiterFormalChecker(tag: String, factory : () => AluMiterIfc) extends AnyFreeSpec with ChiselScalatestTester with TestParams with Formal {

  s"$tag should pass equivalence checks" in {
    val DefaultAnnos = Seq(BoundedCheck(1), Z3EngineAnnotation)
    verify(factory(), DefaultAnnos)
  }
}

class AluMiterFormalCheck extends AluMiterTester("AluMiter_Alu_AluMMX",  () => new AluMiterWithChecker(() => new Alu, () => new AluMMX))

