package med

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._
import scala.util.Random

import scala.collection.mutable
import scala.math


class CellTester( tag : String, factory : () => CellIfc[UInt,UInt]) extends AnyFreeSpec with ChiselScalatestTester {


  s"$tag should work" in {
    test(factory()) { dut =>

      dut.io.row.poke(0)
      dut.io.col.poke(0)

      dut.io.up.poke(0)
      dut.io.lf.poke(0)
      dut.io.dg.poke(0)

      dut.clock.step()

      dut.io.out.expect(0)
//
      dut.io.row.poke(0)
      dut.io.col.poke(1)

      dut.io.up.poke(0)
      dut.io.lf.poke(0)
      dut.io.dg.poke(0)

      dut.clock.step()

      dut.io.out.expect(1)
//
      dut.io.row.poke(0)
      dut.io.col.poke(1)

      dut.io.up.poke(5)
      dut.io.lf.poke(6)
      dut.io.dg.poke(7)

      dut.clock.step()

      dut.io.out.expect(6)
//
      dut.io.row.poke(0)
      dut.io.col.poke(0)

      dut.io.up.poke(255)
      dut.io.lf.poke(6)
      dut.io.dg.poke(7)

      dut.clock.step()

      dut.io.out.expect(7)
    }
  }
}

class CellTest extends CellTester( "CellTest", () => new Cell( UInt(1.W), UInt(8.W))())

