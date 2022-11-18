package alu

import chisel3._

import chisel3.util._

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

  printf("mode=%x opcode=%x a=%x b=%x z=%x\n", mode, opcode, a.asUInt, b.asUInt, z.asUInt)
}

class AluMMX extends AluIfc {
  {
    val ci = WireInit(init=false.B)
    val bn = WireInit(init=b)
    when (opcode === 1.U) {
      ci := true.B
      bn(0) := ~b(0)
      bn(1) := ~b(1)
      bn(2) := ~b(2)
      bn(3) := ~b(3)
    }

    val sa1 = WireInit(init=ci)
    val sb1 = WireInit(init=ci)
    val sa2 = WireInit(init=ci)
    val sb2 = WireInit(init=ci)

    when (mode === 2.U || mode === 4.U) {
      sa1 := false.B
      sb1 := true.B
    }

    when (mode === 4.U) {
      sa2 := false.B
      sb2 := true.B
    }

    val a1 = a(3)  ## sa1 ## a(2)  ## sa2 ## a(1)  ## sa1 ## a(0)
    val b1 = bn(3) ## sb1 ## bn(2) ## sb2 ## bn(1) ## sb1 ## bn(0)

    val z1 = WireInit(UInt(67.W),init=DontCare)

    for { (op, f) <- opcodes } {
      if (op >= 2) {
        when (opcode === op.U) {
          z1 := f(a1, b1)
        }
      }
    }

    when (opcode < 2.U) {
      z1 := a1 + b1 + ci
    }

    z(0) := z1(15,0)
    z(1) := z1(32,17)
    z(2) := z1(49,34)
    z(3) := z1(66,51)

  }

  printf("mode=%x opcode=%x a=%x b=%x z=%x\n", mode, opcode, a.asUInt, b.asUInt, z.asUInt)
}

object Main extends App {
  println(getVerilogString(new Alu))
}

