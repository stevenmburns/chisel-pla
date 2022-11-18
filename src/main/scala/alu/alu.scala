// See README.md for license details.

package alu

import chisel3._

import chisel3.util._

class Alu extends Module {
  val a = IO(Input(Vec(4, UInt(16.W))))
  val b = IO(Input(Vec(4, UInt(16.W))))
  val z = IO(Output(Vec(4, UInt(16.W))))

  val opcode = IO(Input(UInt(4.W)))
  val mode = IO(Input(UInt(3.W)))

  z := DontCare

  for { k <- Seq(1, 2, 4) } {

    when (mode === k.U) {
      val a1 = a.asTypeOf(Vec(4/k, UInt((16*k).W)))
      val b1 = b.asTypeOf(Vec(4/k, UInt((16*k).W)))
      val z1 = Wire(Vec(4/k, UInt((16*k).W)))

      z1 := DontCare

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

/*

  when (mode === 1.U) {
    val a1 = a.asTypeOf(Vec(4, UInt(16.W)))
    val b1 = b.asTypeOf(Vec(4, UInt(16.W)))
    val z1 = Wire(Vec(4, UInt(16.W)))

    z1 := DontCare

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

  } .elsewhen (mode === 2.U) {
    val a2 = a.asTypeOf(Vec(2, UInt(32.W)))
    val b2 = b.asTypeOf(Vec(2, UInt(32.W)))
    val z2 = Wire(Vec(2, UInt(32.W)))

    z2 := DontCare

    when (opcode === 0.U) {
      for { ((aa, bb), zz) <- (a2 zip b2) zip z2 } {
        zz := aa + bb
      }
    } .elsewhen (opcode === 1.U) {
      for { ((aa, bb), zz) <- (a2 zip b2) zip z2 } {
        zz := aa - bb
      }
    } .elsewhen (opcode === 2.U) {
      for { ((aa, bb), zz) <- (a2 zip b2) zip z2 } {
        zz := aa & bb
      }
    } .elsewhen (opcode === 3.U) {
      for { ((aa, bb), zz) <- (a2 zip b2) zip z2 } {
        zz := aa | bb
      }
    }

    z := z2.asTypeOf(Vec(4, UInt(16.W)))

  } .elsewhen (mode === 4.U) {
    val a4 = a.asTypeOf(Vec(1, UInt(64.W)))
    val b4 = b.asTypeOf(Vec(1, UInt(64.W)))
    val z4 = Wire(Vec(1, UInt(64.W)))

    z4 := DontCare

    when (opcode === 0.U) {
      for { ((aa, bb), zz) <- (a4 zip b4) zip z4 } {
        zz := aa + bb
      }
    } .elsewhen (opcode === 1.U) {
      for { ((aa, bb), zz) <- (a4 zip b4) zip z4 } {
        zz := aa - bb
      }
    } .elsewhen (opcode === 4.U) {
      for { ((aa, bb), zz) <- (a4 zip b4) zip z4 } {
        zz := aa & bb
      }
    } .elsewhen (opcode === 3.U) {
      for { ((aa, bb), zz) <- (a4 zip b4) zip z4 } {
        zz := aa | bb
      }
    }

    z := z4.asTypeOf(Vec(4, UInt(16.W)))
  }

 */

  printf("mode=%x opcode=%x a=%x b=%x z=%x\n", mode, opcode, a.asUInt, b.asUInt, z.asUInt)


}

object Main extends App {
  println(getVerilogString(new Alu))
}

