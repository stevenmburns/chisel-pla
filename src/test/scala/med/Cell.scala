package med

import chisel3._
import chisel3.util._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import scala.collection.mutable
import scala.math

class CellTester(c: CellIfc[UInt,UInt]) extends PeekPokeTester(c) {

  poke( c.io.row, 0)
  poke( c.io.col, 0)

  poke( c.io.up, 0)
  poke( c.io.lf, 0)
  poke( c.io.dg, 0)

  step(1)

  expect( c.io.out, 0)
//
  poke( c.io.row, 0)
  poke( c.io.col, 1)

  poke( c.io.up, 0)
  poke( c.io.lf, 0)
  poke( c.io.dg, 0)

  step(1)

  expect( c.io.out, 1)
//
  poke( c.io.row, 0)
  poke( c.io.col, 1)

  poke( c.io.up, 5)
  poke( c.io.lf, 6)
  poke( c.io.dg, 7)

  step(1)

  expect( c.io.out, 6)
//
  poke( c.io.row, 0)
  poke( c.io.col, 0)

  poke( c.io.up, 255)
  poke( c.io.lf, 6)
  poke( c.io.dg, 7)

  step(1)

  expect( c.io.out, 7)

}


class GenericCellTest( tag : String, factory : () => CellIfc[UInt,UInt], backendStr : String)
    extends FlatSpec with Matchers {
  behavior of tag

  it should "work" in {
    chisel3.iotesters.Driver( factory, backendStr) { c =>
      new CellTester(c)
    } should be (true)
  }
}

class CellTestFirrtl extends GenericCellTest( "CellTest", () => new Cell( UInt(1.W), UInt(8.W))(), "firrtl")

