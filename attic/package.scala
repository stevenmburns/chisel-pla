package med

import chisel3._
import chisel3.util._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}



class TSRTester(c: TappedShiftRegisterModule) extends PeekPokeTester(c) {

// c.io.inp
// c.io.out

  poke( c.io.inp, 47)

  expect( c.io.out(0), 47)

  step(1)
  expect( c.io.out(0), 47)
  expect( c.io.out(1), 47)

  poke( c.io.inp, 48)
  expect( c.io.out(0), 48)
  step(1)
  expect( c.io.out(0), 48)
  expect( c.io.out(1), 48)
  expect( c.io.out(2), 47)

  poke( c.io.inp, 49)
  expect( c.io.out(0), 49)
  step(1)
  expect( c.io.out(0), 49)
  expect( c.io.out(1), 49)
  expect( c.io.out(2), 48)
  expect( c.io.out(3), 47)


}


class TSRTest extends FlatSpec with Matchers {
  behavior of "Tapped Shift Register"

  it should "work" in {
    val args = Array("")
    chisel3.iotesters.Driver.execute( args, () => new TappedShiftRegisterModule) { c =>
      new TSRTester(c)
    } should be (true)
  }
}



