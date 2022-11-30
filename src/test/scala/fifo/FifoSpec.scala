package fifo

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class FifoSpecTester(factory : () => FifoIfc[UInt]) extends AnyFreeSpec with ChiselScalatestTester {

  val rnd = new scala.util.Random()
  val hashstr = f"${BigInt(32, rnd)}%8x"

  rnd.setSeed(47L)

  s"Fifo${hashstr} should work" in {
    test(factory()).withAnnotations(Seq(TreadleBackendAnnotation)) { dut =>

      /*
      dut.io.inp.initSource().setSourceClock(dut.clock)
      dut.io.out.initSink().setSinkClock(dut.clock)

      val s = Seq.tabulate(1000){ i => i.U } 

      fork {
        dut.io.inp.enqueueSeq(s)
      } .fork {
        dut.io.out.expectDequeueSeq(s)
      }.join()
       */

      val n = 1000
      val p_i = 0.85
      val p_o = 0.85

      dut.reset.poke(true.B)
      dut.io.inp.valid.poke(false.B)
      dut.io.inp.bits.poke(0xdead.U)
      dut.io.out.ready.poke(false.B)
      dut.clock.step()


      dut.reset.poke(false.B)

      var inp_index = 0
      var out_index = 0

      for {_ <- 0 until n} {

        val g_i = rnd.nextDouble() < p_i
        val g_o = rnd.nextDouble() < p_o

        dut.io.inp.valid.poke(g_i.B)
        dut.io.inp.bits.poke(if (g_i) inp_index.U else 0xdead.U)
        dut.io.out.ready.poke(g_o.B)

        if (g_o && dut.io.out.valid.peek().litToBoolean) {
          // println(s"Dequeueing ${out_index}")
          dut.io.out.bits.expect(out_index.U)
          out_index += 1
        }
        if (g_i && dut.io.inp.ready.peek().litToBoolean) {
          // println(s"Enqueueing ${inp_index}")
          inp_index += 1
        }

        dut.clock.step()
      }
      println(s"final inp_index = ${inp_index} final out_index = ${out_index}")
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

class QueueFifo_8_16SpecTest extends FifoSpecTester(() => new QueueFifo(8, UInt(16.W)))

