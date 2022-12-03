package alu
import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

class AluIfc extends Module {
  val a = IO(Input(Vec(4, UInt(16.W))))
  val b = IO(Input(Vec(4, UInt(16.W))))
  val z = IO(Output(Vec(4, UInt(16.W))))

  val opcode = IO(Input(UInt(2.W)))
  val mode = IO(Input(UInt(3.W)))

  val opcodes = Seq[(Int,(UInt,UInt)=>UInt)](
    (0, {_ + _}),
    (1, {_ - _}),
    (2, {_ & _}),
    (3, {_ | _}))
}

class Alu extends AluIfc {
  z := DontCare

  for { k <- Seq(1, 2, 4) } {
    when (mode === k.U) {
      val a1 = a.asTypeOf(Vec(4/k, UInt((16*k).W)))
      val b1 = b.asTypeOf(Vec(4/k, UInt((16*k).W)))
      val z1 = Wire(Vec(4/k, UInt((16*k).W)))
      z1 := DontCare
      for { (op, f) <- opcodes } {
        when (opcode === op.U) {
          for { ((aa, bb), zz) <- (a1 zip b1) zip z1 } {
            zz := f(aa, bb)
          }
        }
      }
      z := z1.asTypeOf(Vec(4, UInt(16.W)))
    }
  }

  //printf("mode=%x opcode=%x a=%x b=%x z=%x\n", mode, opcode, a.asUInt, b.asUInt, z.asUInt)
}

class KPG extends Bundle {
  val a = Bool()
  val b = Bool()
}

object KPG {
  val kill = (new KPG).Lit(_.a -> false.B, _.b -> false.B)
  val prop = (new KPG).Lit(_.a -> false.B, _.b -> true.B)
  val gen  = (new KPG).Lit(_.a -> true.B,  _.b -> true.B)
}

class AluMMX extends AluIfc {
  z := DontCare

  val ci = WireInit(init=false.B)
  val bn = WireInit(init=b)
  val s1 = WireInit(init=KPG.kill)
  val s2 = WireInit(init=KPG.kill)

  when (opcode === 1.U) {
    ci := true.B
    for {i <- 0 until 4} bn(i) := ~b(i)
    s1 := KPG.gen
    s2 := KPG.gen
  }

  when (mode === 2.U || mode === 4.U) { s1 := KPG.prop }
  when (mode === 4.U) { s2 := KPG.prop }

  val a1 = a(3)  ## s1.a ## a(2)  ## s2.a ## a(1)  ## s1.a ## a(0)
  val b1 = bn(3) ## s1.b ## bn(2) ## s2.b ## bn(1) ## s1.b ## bn(0)

  for { (op, f) <- opcodes } {
    if (op >= 2) {
      when (opcode === op.U) {
        for {((aa, bb), zz) <- (a zip b) zip z} {
          zz := f(aa, bb)
        }
      }
    }
  }

  when (opcode < 2.U) {
    val z1 = a1 + b1 + ci
    for {i <- 0 until 4} z(i) := z1(i*17+15,i*17)
  }

  //printf("mode=%x opcode=%x a=%x b=%x z=%x\n", mode, opcode, a.asUInt, b.asUInt, z.asUInt)
}

// sbt 'runMain alu.MainAlu'
object MainAlu extends App {
  println(getVerilogString(new Alu, Array("--emission-options=disableMemRandomization,disableRegisterRandomization")))
}

// sbt 'runMain alu.MainAluMMX'
object MainAluMMX extends App {
  println(getVerilogString(new AluMMX, Array("--emission-options=disableMemRandomization,disableRegisterRandomization")))
}

