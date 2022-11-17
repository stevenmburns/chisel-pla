// See README.md for license details.

package pla

import chisel3._

import chisel3.util._

class Pla(cubes : Seq[String]) extends Module {
  val n = cubes(0).length

  val inp = IO(Input(UInt(n.W)))
  val out = IO(Output(UInt(1.W)))

  def g( t: (Char, Bool)) = t._1 match {
     case '-' => true.B
     case '0' => ~t._2
     case '1' => t._2
  }

  out := cubes map {_.toArray zip(inp.asBools) map g reduce{_ & _}} reduce {_ | _}
}

object MainXor extends App {
  println(getVerilogString(
    new Pla(Seq(
      "110",
      "010",
      "001",
      "111"
    ))))
}

object MainMaj extends App {
  println(getVerilogString(
    new Pla(Seq(
      "11-",
      "-11",
      "1-1"
    ))))
}
