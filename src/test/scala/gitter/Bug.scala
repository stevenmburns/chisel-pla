package bug

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

import testutil._

class BugSpecTester(tag: String, factory : () => Bug) extends AnyFreeSpec with ChiselScalatestTester with TestParams {

  val rnd = new scala.util.Random()
  rnd.setSeed(47L)

  s"$tag should work" in {
    test(factory()).withAnnotations(annons) { dut =>

      dut.io.memOp.poke(3.U)
      dut.io.off.poke(0.U)
      dut.io.readAddr.poke(0.U)


      for { i <- 0 until 16 } {
        dut.io.writeAddr.poke(i.U)
        dut.io.writeData.poke(i.U)
        dut.clock.step()
      }

      dut.io.memOp.poke(0.U)

      for { i <- 0 until 16 } {
        dut.io.readAddr.poke(i.U)
        dut.clock.step()
        dut.io.readData.expect(i.U)
      }

      dut.io.memOp.poke(1.U)
      dut.io.off.poke(0.U)
      dut.io.writeAddr.poke(0.U)
      dut.io.writeData.poke(255.U)

      dut.clock.step()

      dut.io.memOp.poke(0.U)

      for { i <- 0 until 16 } {
        dut.io.readAddr.poke(i.U)
        dut.clock.step()
        dut.io.readData.expect(i.U)
      }




    }
  }
}

class BugSpecTest extends BugSpecTester("Bug", () => new Bug)

