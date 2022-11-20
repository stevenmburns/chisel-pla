package med

import chisel3._
import chisel3.util._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import scala.collection.mutable
import scala.math


class SimpleMockCellArrayTester(c: CellArrayIfc[SimpleMockData,SimpleMockScore]) extends PeekPokeTester(c) {

  val delay = c.delay
  for { ts <- 0 until (c.delay+20)} {

    println( s"Timestamp: $ts")

    c.io.rows.foreach{ (x) => poke( x.ts, ts)}
    c.io.cols.foreach{ (x) => poke( x.ts, ts)}

    if ( ts >= delay) {
      expect( c.io.out.ts, ts-delay)
    }

    step(1)
  }
}

class GenericSimpleMockCellArrayTest( tag : String, factory : () => CellArrayIfc[SimpleMockData,SimpleMockScore], backendStr : String)
    extends FlatSpec with Matchers {
  behavior of tag

  it should "work" in {
    chisel3.iotesters.Driver( factory, backendStr) { c =>
      new SimpleMockCellArrayTester(c)
    } should be (true)
  }
}

class SimpleMockCellArrayRetimedTestFirrtl( M : Int, N : Int) extends GenericSimpleMockCellArrayTest( "ArrayTest", {
  val (protoD,protoS) = (new SimpleMockData, new SimpleMockScore)
  () => new CellArrayRetimed( M, N, protoD, protoS, (i,j) => new SimpleMockCell( protoD, protoS)( Some( (i, j))), SimpleMockCell.boundary, Some(SimpleMockCell.initD _), Some(SimpleMockCell.initS _))
}, "firrtl")

class SimpleMockCellArrayRetimedTest44Firrtl extends SimpleMockCellArrayRetimedTestFirrtl( 4, 4)





