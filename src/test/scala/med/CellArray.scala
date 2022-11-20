package med

import chisel3._
import chisel3.util._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import scala.collection.mutable
import scala.math

class CellArrayTester(c: CellArrayIfc[UInt,UInt]) extends PeekPokeTester(c) {

  val delay = c.delay

  var ts = 0
  val q = mutable.Queue[(Int,BigInt)]()

  def advance = {
    for ( tup <- q.dequeueFirst( _._1 == ts)) {
      println( s"Dequeing ${tup} ...")
      expect( c.io.out, tup._2)
    }
    step(1)
    ts += 1
  }

  poke( c.io.rows, IndexedSeq.fill( c.M)( BigInt(0)))
  poke( c.io.cols, IndexedSeq.fill( c.N)( BigInt(0)))
  q.enqueue( (ts+delay,math.abs( c.M-c.N)))
  advance

  poke( c.io.rows, IndexedSeq.fill( c.M)( BigInt(0)))
  poke( c.io.cols, IndexedSeq.fill( c.N)( BigInt(1)))
  q.enqueue( (ts+delay,c.M + c.N))
  advance

  poke( c.io.rows, IndexedSeq.tabulate( c.M)( (i) => if ( i % 2 == 0) BigInt(0) else BigInt(1)))
  poke( c.io.cols, IndexedSeq.tabulate( c.N)( (i) => if ( i % 2 == 1) BigInt(0) else BigInt(1)))
  q.enqueue( (ts+delay, if ( c.M == c.N) 2 else math.abs( c.M-c.N)))
  advance

  for ( i <- 0 until delay)
    advance

}


class GenericCellArrayTest( tag : String, factory : () => CellArrayIfc[UInt,UInt], backendStr : String)
    extends FlatSpec with Matchers {
  behavior of tag

  it should "work" in {
    chisel3.iotesters.Driver( factory, backendStr) { c =>
      new CellArrayTester(c)
    } should be (true)
  }
}

class CellArrayTestFirrtl( M : Int, N : Int) extends GenericCellArrayTest( "ArrayTest", () => {
  val (protoD,protoS) = (UInt(1.W),UInt(8.W))
  new CellArray( M, N, protoD, protoS, (i,j) => new Cell( protoD, protoS)(), Cell.boundary)
}, "firrtl")

class CellArrayRetimedTestFirrtl( M : Int, N : Int) extends GenericCellArrayTest( "ArrayTest", () => {
  val (protoD,protoS) = (UInt(1.W),UInt(8.W))
  new CellArrayRetimed( M, N, protoD, protoS, (i,j) => new Cell( protoD, protoS)(), Cell.boundary)
}, "firrtl")


class CellArrayTest11Firrtl extends CellArrayTestFirrtl( 1, 1)
class CellArrayTest22Firrtl extends CellArrayTestFirrtl( 2, 2)
class CellArrayTest44Firrtl extends CellArrayTestFirrtl( 4, 4)
class CellArrayTest88Firrtl extends CellArrayTestFirrtl( 8, 8)

class CellArrayRetimedTest88Firrtl extends CellArrayRetimedTestFirrtl( 8, 8)
class CellArrayRetimedTest1616Firrtl extends CellArrayRetimedTestFirrtl( 16, 16)

class CellArrayTest48Firrtl extends CellArrayTestFirrtl( 4, 8)

class CellArrayTest84Firrtl extends CellArrayTestFirrtl( 8, 4)

class CellArrayTest33Firrtl extends CellArrayTestFirrtl( 3, 3)

class CellArrayTest1616Firrtl extends CellArrayTestFirrtl( 16, 16)

class CellArrayTest3216Firrtl extends CellArrayTestFirrtl( 32, 16)

