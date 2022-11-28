package fifo

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Random

class FifoSpecTester(factory : () => FifoIfc[UInt]) extends AnyFreeSpec with ChiselScalatestTester {

  "Fifo should work" in {
    test(factory()) { dut =>

      dut.io.inp.initSource()
      dut.io.inp.setSourceClock(dut.clock)
      dut.io.out.initSink()
      dut.io.out.setSinkClock(dut.clock)

      val s = Seq.tabulate(1000){ i => i.U } 

      fork {
        dut.io.inp.enqueueSeq(s)
      } .fork {
        dut.io.out.expectDequeueSeq(s)
      }.join()

    }
  }
}

class DecoupledStageSpecTest extends FifoSpecTester(() => new DecoupledStage(UInt(16.W)))
class MooreStageSpecTest extends FifoSpecTester(() => new MooreStage(UInt(16.W)))
class BlockedStageSpecTest extends FifoSpecTester(() => new BlockedStage(UInt(16.W)))
class HalfStageSpecTest extends FifoSpecTester(() => new HalfStage(UInt(16.W)))
class Chain_8_DecoupledStage_16SpecTest extends FifoSpecTester(() => new Chain(8, UInt(16.W), (x: UInt) => new DecoupledStage(x)))
class Chain_8_MooreStage_16SpecTest extends FifoSpecTester(() => new Chain(8, UInt(16.W), (x: UInt) => new MooreStage(x)))
class Chain_8_BlockedStage_16SpecTest extends FifoSpecTester(() => new Chain(8, UInt(16.W), (x: UInt) => new BlockedStage(x)))
class Chain_8_HalfStage_16SpecTest extends FifoSpecTester(() => new Chain(8, UInt(16.W), (x: UInt) => new HalfStage(x)))

