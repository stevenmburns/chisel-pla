// See LICENSE for license details.
//
package fifo
import chisel3._
import chisel3.util._


class FifoIfc[T <: Data](gen: T) extends Module {
  val io = IO(new Bundle { 
    val inp = Flipped(Decoupled( gen ))
    val out = Decoupled( gen )
  })
}

class QueueFifo[T <: Data](n : Int, gen: T) extends FifoIfc(gen) {
  io.out <> Queue(io.inp, n)
  override val desiredName = s"QueueFifo_${n}_${gen.getWidth}"
}

class QueueFifoAlt[T <: Data](n : Int, gen: T) extends FifoIfc(gen) {
  val m = Module(new Queue(gen, 16))
  m.io.enq <> io.inp
  io.out <> m.io.deq
  override val desiredName = s"QueueFifoAlt_${n}_${gen.getWidth}"
}

class BlockedStage[T <: Data](gen: T) extends FifoIfc(gen) {
  val out_valid = RegInit(false.B)
  val out_bits = Reg( gen )

  io.out.valid := out_valid
  io.out.bits  := out_bits

  io.inp.ready := io.out.ready

  when (io.inp.ready) {
    out_valid := io.inp.valid
    out_bits := io.inp.bits
  }
}

class HalfStage[T <: Data](gen: T) extends FifoIfc(gen) {
  val out_valid = RegInit(false.B)
  val out_bits = Reg( gen )

  io.out.valid := out_valid
  io.out.bits  := out_bits

  io.inp.ready := ~out_valid

  when ( ~out_valid) {
    out_valid := io.inp.valid
    out_bits := io.inp.bits
  } .otherwise {
    out_valid := ~io.out.ready
  }
}


class DecoupledStage[T <: Data](gen: T) extends FifoIfc(gen) {
  val out_valid = RegInit(false.B)
  val out_bits = Reg( gen )

  io.out.valid := out_valid
  io.out.bits  := out_bits

  io.inp.ready := io.out.ready | ~io.out.valid

  out_valid := io.inp.valid | ~io.inp.ready
  when ( io.inp.ready) {
     out_bits := io.inp.bits
  }
}

object DecoupledStage {
  def apply[T <: Data](enq: DecoupledIO[T]): DecoupledIO[T]  = {
    val q = Module(new DecoupledStage(enq.bits.cloneType))
    q.io.inp.valid := enq.valid // not using <> so that override is allowed
    q.io.inp.bits := enq.bits
    enq.ready := q.io.inp.ready
    q.io.out
  }
}

class MooreStage[T <: Data](gen: T) extends FifoIfc(gen) {
  val data_aux = Reg( gen.cloneType)
  val out_bits = Reg( gen.cloneType)

  val out_valid = RegInit( false.B) 
  val inp_ready = RegInit( true.B) 

  io.inp.ready := inp_ready
  io.out.valid := out_valid
  io.out.bits  := out_bits

  when        (  inp_ready && !out_valid) {
    when ( io.inp.valid) {
      inp_ready := true.B; out_valid := true.B;
      out_bits := io.inp.bits;
    }
  } .elsewhen (  inp_ready &&  out_valid) {
    when        ( !io.inp.valid && io.out.ready) {
      inp_ready := true.B; out_valid := false.B;
    } .elsewhen (  io.inp.valid && io.out.ready) {
      inp_ready := true.B; out_valid := true.B;
	  out_bits := io.inp.bits;
    } .elsewhen (  io.inp.valid && !io.out.ready) {
      inp_ready := false.B; out_valid := true.B;
      data_aux := io.inp.bits;
    }
  } .elsewhen ( !inp_ready && out_valid) {
    when ( io.out.ready) {
      inp_ready := true.B; out_valid := true.B;
	  out_bits := data_aux;
    }
  }
}

object MooreStage {
  def apply[T <: Data](enq: DecoupledIO[T]): DecoupledIO[T]  = {
    val q = Module(new MooreStage(enq.bits.cloneType))
    q.io.inp.valid := enq.valid // not using <> so that override is allowed
    q.io.inp.bits := enq.bits
    enq.ready := q.io.inp.ready
    q.io.out
  }
}

class ChainAlt[T <: Data](n : Int, gen: T, factory: (T) => FifoIfc[T]) extends FifoIfc(gen) {

  def aux(n : Int, inp : DecoupledIO[T]): DecoupledIO[T] = {
    if (n==0) {
      inp
    } else {
      val u = Module(factory(gen))
      u.io.inp <> inp
      aux(n-1, u.io.out)
    }
  }

  io.out <> aux(n, io.inp)
}

class Chain[T <: Data](n : Int, gen: T, factory: (T) => FifoIfc[T]) extends FifoIfc(gen) {
  var nm = ""
  var cursor = io.inp
  for {_ <- 0 until n} {
    val u = Module(factory(gen))
    if (nm == "") {
      nm = u.name
    }
    u.io.inp <> cursor
    cursor = u.io.out
  }
  io.out <> cursor

  override val desiredName = s"Chain_${n}_${nm}_${gen.getWidth}"
}

object MainDecoupledStage extends App {
  emitVerilog(new DecoupledStage(UInt(16.W)))
}

object MainMooreStage extends App {
  emitVerilog(new MooreStage(UInt(16.W)))
}

object MainBlockedStage extends App {
  emitVerilog(new BlockedStage(UInt(16.W)))
}

object MainHalfStage extends App {
  emitVerilog(new HalfStage(UInt(16.W)))
}

object MainChain_8_DecoupledStage_16 extends App {
  emitVerilog(new Chain(8, UInt(16.W), (x: UInt) => new DecoupledStage(x)))
}

object MainChain_8_MooreStage_16 extends App {
  emitVerilog(new Chain(8, UInt(16.W), (x: UInt) => new MooreStage(x)))
}

object MainChain_8_BlockedStage_16 extends App {
  emitVerilog(new Chain(8, UInt(16.W), (x: UInt) => new BlockedStage(x)))
}

object MainChain_8_HalfStage_16 extends App {
  emitVerilog(new Chain(8, UInt(16.W), (x: UInt) => new HalfStage(x)))
}

object MainQueueFifo_8_16 extends App {
  emitVerilog(new QueueFifo(8, UInt(16.W)))
}
