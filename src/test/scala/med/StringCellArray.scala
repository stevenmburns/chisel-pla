package med

import chisel3._
import chisel3.util._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import scala.collection.mutable
import scala.math


class StringCellArrayTester(c: CellArrayIfc[UInt,UInt]) extends PeekPokeTester(c) {

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

  val rStr = "intention"
  val cStr = "execution"

  assert( c.M == rStr.length)
  assert( c.N == cStr.length)

  poke( c.io.rows, IndexedSeq.tabulate( c.M)( (i) => BigInt( rStr(i))))
  poke( c.io.cols, IndexedSeq.tabulate( c.N)( (i) => BigInt( cStr(i))))
  q.enqueue( (ts+delay, 8))
  advance

  for ( i <- 0 until delay)
    advance

}


class GenericStringCellArrayTest( tag : String, factory : () => CellArrayIfc[UInt,UInt], backendStr : String)
    extends FlatSpec with Matchers {
  behavior of tag

  it should "work" in {
    chisel3.iotesters.Driver( factory, backendStr) { c =>
      new StringCellArrayTester(c)
    } should be (true)
  }
}


class StringCellArrayTestFirrtl extends GenericStringCellArrayTest( "StringCellArrayTest", () => {
  val (protoD,protoS) = (UInt(7.W),UInt(8.W))
  new CellArray( 9, 9, protoD, protoS, (i,j) => new Cell( protoD, protoS)(), Cell.boundary)
}, "firrtl")








