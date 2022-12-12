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

class QueueFifoAlt[T <: Data](n : Int, gen: T) extends FifoIfc(gen) {
  val m = Module(new Queue(gen, n))
  m.io.enq.valid := io.inp.valid
  m.io.enq.bits := io.inp.bits
  io.inp.ready := m.io.enq.ready

  io.out <> m.io.deq
  override val desiredName = s"QueueFifoAlt_${n}_${gen.getWidth}"
}

class QueueFifo[T <: Data](n : Int, gen: T) extends FifoIfc(gen) {
  io.out <> Queue(io.inp, n)
  override val desiredName = s"QueueFifo_${n}_${gen.getWidth}"
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

class Credit[T <: Data](n: Int, gen: T, factory : (T) => FifoIfc[T]) extends FifoIfc(gen) {

  val fifo = Module( new Chain(n, gen, factory))

  override val desiredName = s"Credit_${fifo.name}"

  fifo.io.inp <> io.inp

  val credits = RegInit( init=n.U)

  when (         fifo.io.inp.fire && !io.out.fire) {
    credits := credits - 1.U
  } .elsewhen ( !fifo.io.inp.fire &&  io.out.fire) {
    credits := credits + 1.U
  }

  val queue = Module( new Queue( io.out.bits.cloneType, n, flow=true))

  queue.io.enq <> fifo.io.out

  io.out <> queue.io.deq

// The challenge is to ensure that queue.io.enq.ready is always true
//    by controlling (deasserting more often) fifo.io.inp.valid and io.inp.ready
// I weakened the assertion with the last valid is false (causing all the readys (enables) to be true)
  assert( queue.io.enq.ready || !fifo.io.out.valid)

  when ( credits === 0.U && !io.out.fire) {
    io.inp.ready := false.B
    fifo.io.inp.valid := false.B
  }

// Reap the benefits
  fifo.io.out.ready := true.B

//  printf( "credits: %d mValid: %d mReady: %d\n", credits, fifo.io.out.valid, queue.io.enq.ready)

}

class CreditSplit[T <: Data](n: Int, gen: T) extends FifoIfc(gen) {
  override val desiredName = s"CreditSplit_${n}_${gen.getWidth}"

  val queue = Module( new Queue( gen.cloneType, n, flow=true))
  val credit_pipe_out = ShiftRegister(io.out.fire, n, false.B, true.B) // in, n, resetData, en
  val data_pipe = Module(new Pipe(io.inp.bits.cloneType, n))

  val credits = RegInit( init=n.U)
  when (         io.inp.fire && !credit_pipe_out) {
    credits := credits - 1.U
  } .elsewhen ( !io.inp.fire &&  credit_pipe_out) {
    credits := credits + 1.U
  }

  queue.io.enq.valid := data_pipe.io.deq.valid
  queue.io.enq.bits := data_pipe.io.deq.bits

  assert( queue.io.enq.ready || !data_pipe.io.deq.valid)

  io.out <> queue.io.deq

  data_pipe.io.enq.valid :=  io.inp.valid
  data_pipe.io.enq.bits :=  io.inp.bits

  io.inp.ready := true.B

  when ( credits === 0.U && !credit_pipe_out) {
    io.inp.ready := false.B
    data_pipe.io.enq.valid := false.B
  }

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

object MainCredit_Chain_8_DecoupledStage_16 extends App {
  emitVerilog(new Credit(8, UInt(16.W), (x: UInt) => new DecoupledStage(x)))
}

object MainCreditSplit_8_16 extends App {
  emitVerilog(new CreditSplit(8, UInt(16.W)))
}
