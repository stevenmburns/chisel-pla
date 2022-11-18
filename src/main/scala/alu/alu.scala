// See README.md for license details.

package alu

import chisel3._

import chisel3.util._

class AluIfc extends Module {
  val a = IO(Input(Vec(4, UInt(16.W))))
  val b = IO(Input(Vec(4, UInt(16.W))))
  val z = IO(Output(Vec(4, UInt(16.W))))

  val opcode = IO(Input(UInt(4.W)))
  val mode = IO(Input(UInt(3.W)))
}


class Alu extends AluIfc {

  val opcodes = Seq[(Int,(UInt,UInt)=>UInt)](
    (0, {_ + _}),
    (1, {_ - _}),
    (2, {_ & _}),
    (3, {_ | _}))

  z := DontCare

  for { k <- Seq(1, 2, 4) } {

    when (mode === k.U) {
      val a1 = a.asTypeOf(Vec(4/k, UInt((16*k).W)))
      val b1 = b.asTypeOf(Vec(4/k, UInt((16*k).W)))
      val z1 = Wire(Vec(4/k, UInt((16*k).W)))

      z1 := DontCare

      for { (op, f) <- opcodes } {
        for { ((aa, bb), zz) <- (a1 zip b1) zip z1 } {
          zz := f(aa, bb)
        }
      }


      when (opcode === 0.U) {
        for { ((aa, bb), zz) <- (a1 zip b1) zip z1 } {
          zz := aa + bb
        }
      } .elsewhen (opcode === 1.U) {
        for { ((aa, bb), zz) <- (a1 zip b1) zip z1 } {
          zz := aa - bb
        }
      } .elsewhen (opcode === 2.U) {
        for { ((aa, bb), zz) <- (a1 zip b1) zip z1 } {
          zz := aa & bb
        }
      } .elsewhen (opcode === 3.U) {
        for { ((aa, bb), zz) <- (a1 zip b1) zip z1 } {
          zz := aa | bb
        }
      }

      z := z1.asTypeOf(Vec(4, UInt(16.W)))

    }
  }


  printf("mode=%x opcode=%x a=%x b=%x z=%x\n", mode, opcode, a.asUInt, b.asUInt, z.asUInt)


}

object Main extends App {
  println(getVerilogString(new Alu))
}

