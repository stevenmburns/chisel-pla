package pla

import chisel3._
import chisel3.util.PopCount
import chiseltest._
import chiseltest.formal._
import org.scalatest.freespec.AnyFreeSpec
import testutil._


class PlaSpecModule(cubes : Seq[String], foo: UInt => Bool) extends Module {
  val dut = Module(new Pla(cubes))
  val inp = IO(Input(chiselTypeOf(dut.inp)))
  dut.inp := inp
  val expected = foo(inp)
  val actual = dut.out
  assert(expected === actual, "%x != %x", expected, actual)
}


class PlaFormalSpec extends  AnyFreeSpec with ChiselScalatestTester with TestParams with Formal {
  val DefaultAnnos = Seq(BoundedCheck(1), Z3EngineAnnotation)

  "Maj should calculate 3-input majority" in {
    verify(new PlaSpecModule(Seq("11-", "-11", "1-1"), inp => PopCount(inp) > 1.U), DefaultAnnos)
  }

  "Xor should calculate three input parity" in {
    verify(new PlaSpecModule(Seq("100", "010", "001", "111"), inp => PopCount(inp).extract(0)), DefaultAnnos)
  }
}
