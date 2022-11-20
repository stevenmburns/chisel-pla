package med

import fev.{ RunConformal}

import chisel3._
import chisel3.util._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import scala.collection.mutable
import scala.math


class RunConformal1616 extends FlatSpec with Matchers {
  it should "prove equivalence" in {
    RunConformal(
      {
        val (protoD,protoS) = (UInt(2.W),UInt(8.W))
          () => new CellArray( 16, 16, protoD, protoS, (i,j) => new Cell( protoD, protoS)( Some((i, j))), Cell.boundary)
      },
      {
        val (protoD,protoS) = (UInt(2.W),UInt(8.W))
          () => new CellArrayRetimed( 16, 16, protoD, protoS, (i,j) => new Cell( protoD, protoS)( Some((i, j))), Cell.boundary)
      }
    ) should be (true)
  }
}

class RunConformalCellCell2 extends FlatSpec with Matchers {
  it should "prove equivalence" in {
    RunConformal(
      {
        val (protoD,protoS) = (UInt(2.W),UInt(8.W))
          () => new CellFixture( () => new Cell( protoD, protoS)(), protoD, protoS)
      },
      {
        val (protoD,protoS) = (UInt(2.W),UInt(8.W))
          () => new CellFixture( () => new Cell2( protoD, protoS)(), protoD, protoS)
      }
    ) should be (true)
  }
}
