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

  def dontCare[A <: Data](tpe: A): A = WireInit(tpe, DontCare)

  val mem = Mem(16, Vec(4, UInt(8.W)))

  val shifted = MuxLookup(
    io.off, 
    io.writeData.asTypeOf(Vec(4, UInt(8.W))),
    Seq( 
      1.U -> VecInit(dontCare(UInt(8.W)), io.writeData(7,0),
                     dontCare(UInt(8.W)), dontCare(UInt(8.W))),
      2.U -> VecInit(dontCare(UInt(8.W)), dontCare(UInt(8.W)),
                     io.writeData(7,0), io.writeData(15,8)),
      3.U -> VecInit(dontCare(UInt(8.W)), dontCare(UInt(8.W)),
                     dontCare(UInt(8.W)), io.writeData(7,0))
    )
  )

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

/*
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
 */

 /*
  val shifted = WireInit(io.writeData.asTypeOf(Vec(4, UInt(8.W))))
  when (io.off === 1.U) {
    shifted(1) := io.writeData(7,0)
  } .elsewhen ( io.off === 2.U) {
    shifted(2) := io.writeData(7,0)
    shifted(3) := io.writeData(15,8)
  } .elsewhen ( io.off === 3.U) {
    shifted(3) := io.writeData(7,0)
  }
 */

/*
  val shifted = WireInit(io.writeData.asTypeOf(Vec(4, UInt(8.W))))
  shifted(io.off) := io.writeData(7,0)
  when ( io.off === 2.U) {
    shifted(3) := io.writeData(15,8)
  }
 */

  mem.write(io.writeAddr, shifted, mask.asBools)

  io.readData := mem.read(io.readAddr).asUInt


}

object MainBug extends App {
  println(getVerilogString(new Bug, Array("--emission-options=disableMemRandomization,disableRegisterRandomization")))
}
