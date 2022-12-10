package parallel_prefix

import chisel3._
import chiseltest._
import chiseltest.formal._
import org.scalatest.freespec.AnyFreeSpec
import testutil._

import chisel3.experimental.BundleLiterals._

class ParallelPrefixMiter[T <: Data](gen: T, n: Int,
                                     factory0: () => ParallelPrefix[T],
                                     factory1: () => ParallelPrefix[T]) extends
    ParallelPrefix(gen, n) {

  val u0 = Module(factory0())
  val u1 = Module(factory1())
  u0.io.inp := io.inp
  u1.io.inp := io.inp
  io.out := u0.io.out

  val same = u0.io.out === u1.io.out

  assert(same)

}

class AdderMiter(n: Int, factory0: () => AdderIfc, factory1: () => AdderIfc) extends AdderIfc(n) {
  val u0 = Module(factory0())
  val u1 = Module(factory1())
  u0.io.a := io.a
  u1.io.a := io.a
  u0.io.b := io.b
  u1.io.b := io.b

  io.z := u0.io.z

  val same = u0.io.z === u1.io.z

  assert(same)
}

class PriorityEncoderMiter(n: Int, factory0: () => PriorityEncoderIfc, factory1: () => PriorityEncoderIfc) extends PriorityEncoderIfc(n) {
  val u0 = Module(factory0())
  val u1 = Module(factory1())
  u0.io.a := io.a
  u1.io.a := io.a
  io.z := u0.io.z

  val same = u0.io.z === u1.io.z
  assert(same)
}

class AdderSpecTester(tag: String, factory : () => Adder) extends AnyFreeSpec with ChiselScalatestTester with TestParams {

  s"$tag should pass AdderSpec tests" in {
    test(factory()).withAnnotations(annons) { dut =>

      val rnd = new scala.util.Random()

      for {_ <- 0 until 1000} {
        val a = BigInt(dut.n, rnd)
        val b = BigInt(dut.n, rnd)
        val z = (a + b) & ((BigInt(1)<<dut.n) - 1)

        dut.io.a.poke(a.U)
        dut.io.b.poke(b.U)
        dut.clock.step()
        dut.io.z.expect(z.U)
      }
    }
  }
}

class PriorityEncoderSpecTester(tag: String, factory : () => PriorityEncoder) extends AnyFreeSpec with ChiselScalatestTester with TestParams {

  s"$tag should pass PriorityEncoderSpec tests" in {
    test(factory()).withAnnotations(annons) { dut =>

      val rnd = new scala.util.Random()

      for {_ <- 0 until 1000} {
        val a = BigInt(dut.n, rnd)
        // 00101000
        // 00100111
        val z = a & ~(a-1)
        dut.io.a.poke(a.U)
        dut.clock.step()
        dut.io.z.expect(z.U)
      }
    }
  }
}


class AdderSpecKoggeStoneTest extends AdderSpecTester("AdderKoggeStone", () => new Adder(18, () => new KoggeStone(new KPG(), 18, KPG.op(_, _))))
class AdderSpecBrentKungTest extends AdderSpecTester("AdderBruntKung", () => new Adder(18, () => new BrentKung(new KPG(), 18, KPG.op(_, _))))
class AdderSpecHanCarlsonTest extends AdderSpecTester("AdderHanCarlson", () => new Adder(18, () => new HanCarlson(new KPG(), 18, KPG.op(_, _))))
class AdderSpecSklanskyFlatTest extends AdderSpecTester("AdderSklanskyFlat", () => new Adder(18, () => new SklanskyFlat(new KPG(), 18, KPG.op(_, _))))
class AdderSpecSklanskyTest extends AdderSpecTester("AdderSklansky", () => new Adder(18, () => new Sklansky(new KPG(), 18, KPG.op(_, _))))
class AdderSpecSerialTest extends AdderSpecTester("AdderSerial", () => new Adder(18, () => new Serial(new KPG(), 18, KPG.op(_, _))))

class PriorityEncoderSpecTest extends PriorityEncoderSpecTester("PriorityEncoder18", () => new PriorityEncoder(18, () => new SklanskyFlat(Bool(), 18, {(a: Bool, b: Bool) => a | b})))


class ParallelPrefixOrChecker extends AnyFreeSpec with ChiselScalatestTester with Formal {
  val DefaultAnnos = Seq(BoundedCheck(1), Z3EngineAnnotation)
  val tag = "SerialOr and KoggeStoneOr"
  val gen = Bool()
  val n = 64
  val op = {(x: Bool, y: Bool) => (x | y)}
  val factory0 = () => new Serial(gen, n, op)
  val factory1 = () => new KoggeStone(gen, n, op)
  s"$tag should be formally equivalent" in {
    verify(new ParallelPrefixMiter(gen, n, factory0, factory1), DefaultAnnos)
  }
}

class ParallelPrefixKPGChecker extends AnyFreeSpec with ChiselScalatestTester with Formal {
  val DefaultAnnos = Seq(BoundedCheck(1), Z3EngineAnnotation)
  val tag = "SerialKPG and HanCarlsonKPG"
  val gen = new KPG()
  val n = 64
  val op = KPG.op(_, _)
  val factory0 = () => new Serial(gen, n, op)
  val factory1 = () => new HanCarlson(gen, n, op)
  s"$tag should be formally equivalent" in {
    verify(new ParallelPrefixMiter(gen, n, factory0, factory1), DefaultAnnos)
  }
}

class PriorityEncoderChecker extends AnyFreeSpec with ChiselScalatestTester with Formal {
  val DefaultAnnos = Seq(BoundedCheck(1), Z3EngineAnnotation)
  val tag = "SerialPriorityEncoder and SklanskyFlatPriorityEncoder"
  val n = 5
  val op = (a: Bool, b: Bool) => a | b
  val factory0 = () => new PriorityEncoderSimple(n)
  val factory1 = () => new PriorityEncoder(5, () => new SklanskyFlat(Bool(), 5, op))
  s"$tag should be formally equivalent" in {
    verify(new PriorityEncoderMiter(n, factory0, factory1), DefaultAnnos)
  }
}


class AdderChecker extends AnyFreeSpec with ChiselScalatestTester with Formal {
  val DefaultAnnos = Seq(BoundedCheck(1), Z3EngineAnnotation)
  val tag = "SerialAdder and HanCarlsonAdder"
  val n = 5
  val factory0 = () => new AdderSimple(n)
  val factory1 = () => new Adder(5, () => new HanCarlson(new KPG(), 5, KPG.op(_, _)))
  s"$tag should be formally equivalent" in {
    verify(new AdderMiter(n, factory0, factory1), DefaultAnnos)
  }
}
