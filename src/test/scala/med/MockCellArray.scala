package med

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._
import scala.util.Random
import scala.collection.mutable
import scala.math


class MockCellArrayTester( tag : String, factory : () => CellArrayIfc[MockData,MockScore]) extends AnyFreeSpec with ChiselScalatestTester {

  s"$tag should work" in {
    test(factory()) { dut =>
      val delay = dut.delay

      for { ts <- 0 until (delay+20)} {

        println( s"Timestamp: $ts")

        for {i<-0 until dut.M} {
          dut.io.rows(i).ts.poke(ts)
          dut.io.rows(i).i.poke(i)
          dut.io.rows(i).j.poke(-1)
        }

        for {j<-0 until dut.N} {
          dut.io.cols(j).ts.poke(ts)
          dut.io.cols(j).i.poke(-1)
          dut.io.cols(j).j.poke(j)
        }

        if ( ts >= delay) {
          dut.io.out.ts.expect(ts-delay)
          dut.io.out.i.expect(dut.M-1)
          dut.io.out.j.expect(dut.N-1)
        }

        dut.clock.step()
      }
    }
  }
}

class MockCellArrayTest( M : Int, N : Int) extends MockCellArrayTester( "MockCellArrayTest", {
  val (protoD,protoS) = (new MockData, new MockScore)
  () => new CellArray( M, N, protoD.cloneType, protoS.cloneType, (i, j) => new MockCell( protoD.cloneType, protoS.cloneType)( Some( (i, j))), MockCell.boundary)
})

class MockCellArrayTest44 extends MockCellArrayTest( 4, 4)

class MockCellArrayRetimedTest( M : Int, N : Int) extends MockCellArrayTester( "MockCellArrayRetimedTest", {
  val (protoD,protoS) = (new MockData, new MockScore)
  () => new CellArrayRetimed( M, N, protoD.cloneType, protoS.cloneType, (i,j) => new MockCell( protoD.cloneType, protoS.cloneType)( Some( (i, j))), MockCell.boundary, Some(MockCell.initD _), Some(MockCell.initS _))
})

class MockCellArrayRetimedTest44 extends MockCellArrayRetimedTest( 4, 4)






