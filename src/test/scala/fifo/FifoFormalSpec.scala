package fifo

import chisel3._
import chiseltest._
import chiseltest.formal._
import org.scalatest.freespec.AnyFreeSpec
import testutil._



class FifoFormalSpecModule(depth: Int, factory : () => FifoIfc[UInt]) extends Module {
  // instantiate DUT and create wrapper
  val dut = Module(factory())
  val io = IO(chiselTypeOf(dut.io)) ; io <> dut.io
  // add assertions
  MagicPacketTracker(dut.io.inp, dut.io.out, depth, debugPrint = true)
}


class FifoFormalSpec(tag: String, factory : () => FifoIfc[UInt], depth: Int) extends AnyFreeSpec with ChiselScalatestTester with TestParams with Formal {
  val DefaultAnnos = Seq(BoundedCheck(depth * 2 + 1), BtormcEngineAnnotation)
  s"$tag should work" in {
    verify(new FifoFormalSpecModule(depth, factory), DefaultAnnos)
  }
}




class DecoupledStageSpecFormal extends FifoFormalSpec ("DecoupledStage", () => new DecoupledStage(UInt(16.W)), 1)
class MooreStageSpecFormal extends FifoFormalSpec ("MooreStage", () => new MooreStage(UInt(16.W)), 2)
class BlockedStageSpecFormal extends FifoFormalSpec ("BlockedStage", () => new BlockedStage(UInt(16.W)), 1)
class HalfStageSpecFormal extends FifoFormalSpec ("HalfStage", () => new HalfStage(UInt(16.W)), 1)
class Chain_8_DecoupledStage_16SpecFormal extends FifoFormalSpec ("Chain_8_DecoupledStage", () => new Chain(8, UInt(16.W), (x: UInt) => new DecoupledStage(x)), depth = 8)
// 16 is a little deep for a quick formal check, it would take more than 1h on my laptop, thus we reduced the chain depth to 3
class Chain_3_MooreStage_16SpecFormal extends FifoFormalSpec ("Chain_3_MooreStage", () => new Chain(3, UInt(16.W), (x: UInt) => new MooreStage(x)), depth = 3 * 2)
// depth reduced to 5 to make formal check faster
class Chain_5_BlockedStage_16SpecFormal extends FifoFormalSpec ("Chain_5_BlockedStage", () => new Chain(5, UInt(16.W), (x: UInt) => new BlockedStage(x)), depth = 5)
class Chain_8_HalfStage_16SpecFormal extends FifoFormalSpec ("Chain_8_HalfStage", () => new Chain(8, UInt(16.W), (x: UInt) => new HalfStage(x)), depth = 8)

// depth reduced to make formal check faster
class QueueFifo_5_16SpecFormal extends FifoFormalSpec ("QueueFifo_5", () => new QueueFifo(5, UInt(16.W)), depth = 5)
class QueueFifoAlt_5_16SpecFormal extends FifoFormalSpec ("QueueFifoAlt_5", () => new QueueFifoAlt(5, UInt(16.W)), depth = 5)
