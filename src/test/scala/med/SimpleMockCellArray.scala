package med

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._
import scala.util.Random
import scala.collection.mutable
import scala.math


class GenericSimpleMockCellArrayTest( tag : String, factory : () => CellArrayIfc[SimpleMockData,SimpleMockScore])
    extends AnyFreeSpec with ChiselScalatestTester {

  s"$tag should with SimpleMockCellArrayTest" in {
    test(factory()).withAnnotations(Seq(TreadleBackendAnnotation)) { dut =>
      val delay = dut.delay
      for { ts <- 0 until (dut.delay+20)} {

        println( s"Timestamp: $ts")

        dut.io.rows.foreach{ (x) => x.ts.poke(ts)}
        dut.io.cols.foreach{ (x) => x.ts.poke(ts)}
        
        if ( ts >= delay) {
          dut.io.out.ts.expect(ts-delay)
        }

        dut.clock.step()
      }
    }
  }
}

class SimpleMockCellArrayRetimedTestFirrtl( M : Int, N : Int) extends GenericSimpleMockCellArrayTest( s"CellArrayRetimed_${M}_${N}", {
  val (protoD,protoS) = (new SimpleMockData, new SimpleMockScore)
  () => new CellArrayRetimed( M, N, protoD, protoS, (i,j) => new SimpleMockCell( protoD, protoS)( Some( (i, j))), SimpleMockCell.boundary, Some(SimpleMockCell.initD _), Some(SimpleMockCell.initS _))
})

class SimpleMockCellArrayRetimedTest44Firrtl extends SimpleMockCellArrayRetimedTestFirrtl( 4, 4)





