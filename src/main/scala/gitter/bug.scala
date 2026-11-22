package bug

import chisel3._
import chisel3.util._

class Bug extends Module {

  val io = IO(new Bundle {
    val memOp = Input(UInt(2.W))
    val off = Input(UInt(2.W))
    val writeAddr = Input(UInt(4.W))
    val writeData = Input(UInt(32.W))
    val readAddr = Input(UInt(4.W))
    val readData = Output(UInt(32.W))
  })

  val mem = SyncReadMem(16, Vec(4, UInt(8.W)))

  val lookup = MuxLookup(
    io.memOp,
    0.U,
    Seq(
      1.U -> "b0001".U,
      2.U -> "b0011".U,
      3.U -> "b1111".U
    )
  )

  val mask = lookup.rotateLeft(io.off)

  val wD = io.writeData.asTypeOf(Vec(4, UInt(8.W)))
  val shifted : Vec[UInt] = MuxLookup(
    io.off, 
    wD,
    Seq( 
      0.U -> VecInit(wD(0), wD(1), wD(2), wD(3)),
      1.U -> VecInit(wD(0), wD(0), wD(2), wD(3)),
      2.U -> VecInit(wD(0), wD(1), wD(0), wD(1)),
      3.U -> VecInit(wD(0), wD(1), wD(2), wD(0))
    )
  )

  mem.write(io.writeAddr, shifted, mask.asBools)
  io.readData := mem.read(io.readAddr).asUInt

}

object MainBug extends App {
  emitVerilog(new Bug, Array("--emission-options=disableMemRandomization,disableRegisterRandomization", "--repl-seq-mem", "-o:mems.txt", "--gen-mem-verilog", "full", "--infer-rw"))
}
