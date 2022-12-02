package med

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._
import scala.util.Random
import scala.collection.mutable
import scala.math


class CellArrayTester( tag : String, factory : () => CellArrayIfc[UInt,UInt]) extends AnyFreeSpec with ChiselScalatestTester {

  s"$tag should work" in {
    test(factory()).withAnnotations(Seq(TreadleBackendAnnotation)) { dut =>

      val delay = dut.delay

      var ts = 0
      val q = mutable.Queue[(Int,BigInt)]()

      def advance = {
        for ( tup <- q.dequeueFirst( _._1 == ts)) {
          println( s"Dequeueing ${tup} ...")
          dut.io.out.expect(tup._2)
        }
        dut.clock.step()
        ts += 1
      }

      for {i <- 0 until dut.M} dut.io.rows(i).poke(BigInt(0))
      for {j <- 0 until dut.N} dut.io.cols(j).poke(BigInt(0))

      q.enqueue( (ts+delay,math.abs( dut.M-dut.N)))
      advance

      for {i <- 0 until dut.M} dut.io.rows(i).poke(BigInt(0))
      for {j <- 0 until dut.N} dut.io.cols(j).poke(BigInt(1))

      q.enqueue( (ts+delay,dut.M + dut.N))
      advance

      for {i <- 0 until dut.M} dut.io.rows(i).poke(if ( i % 2 == 0) BigInt(0) else BigInt(1))
      for {j <- 0 until dut.N} dut.io.cols(j).poke(if ( j % 2 == 1) BigInt(0) else BigInt(1))

      q.enqueue( (ts+delay, if ( dut.M == dut.N) 2 else math.abs( dut.M-dut.N)))
      advance

      for ( i <- 0 until delay)
        advance
    }
  }
}

class CellArrayTest( M : Int, N : Int) extends CellArrayTester( s"ArrayTest_${M}_${N}", () => {
  val (protoD,protoS) = (UInt(1.W),UInt(8.W))
  new CellArray( M, N, protoD, protoS, (i,j) => new Cell( protoD, protoS)(), Cell.boundary)
})

class CellArrayRetimedTest( M : Int, N : Int) extends CellArrayTester( s"ArrayRetimedTest_${M}_${N}", () => {
  val (protoD,protoS) = (UInt(1.W),UInt(8.W))
  new CellArrayRetimed( M, N, protoD, protoS, (i,j) => new Cell( protoD, protoS)(), Cell.boundary)
})


class CellArrayTest11 extends CellArrayTest( 1, 1)
class CellArrayTest22 extends CellArrayTest( 2, 2)
class CellArrayTest44 extends CellArrayTest( 4, 4)
class CellArrayTest88 extends CellArrayTest( 8, 8)

class CellArrayRetimedTest88 extends CellArrayRetimedTest( 8, 8)
class CellArrayRetimedTest1616 extends CellArrayRetimedTest( 16, 16)

class CellArrayTest48 extends CellArrayTest( 4, 8)

class CellArrayTest84 extends CellArrayTest( 8, 4)

class CellArrayTest33 extends CellArrayTest( 3, 3)

class CellArrayTest1616 extends CellArrayTest( 16, 16)

class CellArrayTest3216 extends CellArrayTest( 32, 16)

