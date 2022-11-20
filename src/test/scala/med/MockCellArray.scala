package med

import chisel3._
import chisel3.util._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import scala.collection.mutable
import scala.math


class MockCellArrayTester(c: CellArrayIfc[MockData,MockScore]) extends PeekPokeTester(c) {

  val delay = c.delay

  for { ts <- 0 until (delay+20)} {

    println( s"Timestamp: $ts")

    c.io.rows.zipWithIndex.foreach{ case (x,i) => poke( x.ts, ts); poke( x.i, i); poke( x.j, -1)}
    c.io.cols.zipWithIndex.foreach{ case (x,j) => poke( x.ts, ts); poke( x.i, -1); poke( x.j, j)}

    if ( ts >= delay) {
      expect( c.io.out.ts, ts-delay)
      expect( c.io.out.i, c.M-1)
      expect( c.io.out.j, c.N-1)
    }

    step(1)
  }
}

class GenericMockCellArrayTest( tag : String, factory : () => CellArrayIfc[MockData,MockScore], backendStr : String)
    extends FlatSpec with Matchers {
  behavior of tag

  it should "work" in {
    chisel3.iotesters.Driver( factory, backendStr) { c =>
      new MockCellArrayTester(c)
    } should be (true)
  }
}

class MockCellArrayTestFirrtl( M : Int, N : Int) extends GenericMockCellArrayTest( "ArrayTest", {
  val (protoD,protoS) = (new MockData, new MockScore)
  () => new CellArray( M, N, protoD.cloneType, protoS.cloneType, (i, j) => new MockCell( protoD.cloneType, protoS.cloneType)( Some( (i, j))), MockCell.boundary)
}, "firrtl")

class MockCellArrayTest44Firrtl extends MockCellArrayTestFirrtl( 4, 4)

class MockCellArrayRetimedTestFirrtl( M : Int, N : Int) extends GenericMockCellArrayTest( "ArrayTest", {
  val (protoD,protoS) = (new MockData, new MockScore)
  () => new CellArrayRetimed( M, N, protoD.cloneType, protoS.cloneType, (i,j) => new MockCell( protoD.cloneType, protoS.cloneType)( Some( (i, j))), MockCell.boundary, Some(MockCell.initD _), Some(MockCell.initS _))
}, "firrtl")

class MockCellArrayRetimedTest44Firrtl extends MockCellArrayRetimedTestFirrtl( 4, 4)






