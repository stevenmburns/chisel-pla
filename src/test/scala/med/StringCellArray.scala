package med

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._
import scala.util.Random

import scala.collection.mutable
import scala.math


class StringCellArrayTester( tag : String, factory : () => CellArrayIfc[UInt,UInt]) extends AnyFreeSpec with ChiselScalatestTester {

  s"$tag should work with StringCellArrayTester" in {
    test(factory()).withAnnotations(Seq(TreadleBackendAnnotation)) { dut =>

      val delay = dut.delay

      var ts = 0
      val q = mutable.Queue[(Int,BigInt)]()

      def advance = {
        for ( tup <- q.dequeueFirst( _._1 == ts)) {
          println( s"Dequeing ${tup} ...")
          dut.io.out.expect(tup._2)
        }
        dut.clock.step()
        ts += 1
      }

      val rStr = "intention"
      val cStr = "execution"

      assert( dut.M == rStr.length)
      assert( dut.N == cStr.length)

      for {i <- 0 until dut.M} dut.io.rows(i).poke(BigInt(rStr(i)))
      for {j <- 0 until dut.N} dut.io.cols(j).poke(BigInt(cStr(j)))

      q.enqueue( (ts+delay, 8))
      advance

      for ( i <- 0 until delay)
        advance
    }
  }
}


class StringCellArrayTest extends StringCellArrayTester( "StringCellArray", () => {
  val (protoD,protoS) = (UInt(7.W),UInt(8.W))
  new CellArray( 9, 9, protoD, protoS, (i,j) => new Cell( protoD, protoS)(), Cell.boundary)
})








