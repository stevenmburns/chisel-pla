// See README.md for license details.

package pla

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._

class MajSpec extends AnyFreeSpec with ChiselScalatestTester {

  "Maj should calculate 3-input majority" in {
    test(new Pla(Seq("11-", "-11", "1-1"))).withAnnotations(Seq(TreadleBackendAnnotation)) { dut =>

      dut.inp.poke(0)
      dut.clock.step()
      dut.out.expect(0)

      dut.inp.poke(1)
      dut.clock.step()
      dut.out.expect(0)

      dut.inp.poke(2)
      dut.clock.step()
      dut.out.expect(0)

      dut.inp.poke(4)
      dut.clock.step()
      dut.out.expect(0)

      dut.inp.poke(3)
      dut.clock.step()
      dut.out.expect(1)

      dut.inp.poke(5)
      dut.clock.step()
      dut.out.expect(1)

      dut.inp.poke(6)
      dut.clock.step()
      dut.out.expect(1)

      dut.inp.poke(7)
      dut.clock.step()
      dut.out.expect(1)

    }
  }
}

class XorSpec extends AnyFreeSpec with ChiselScalatestTester {

  "Xor should calculate three input parity" in {
    test(new Pla(Seq("100", "010", "001", "111"))).withAnnotations(Seq(TreadleBackendAnnotation)) { dut =>

      dut.inp.poke(0)
      dut.clock.step()
      dut.out.expect(0)

      dut.inp.poke(1)
      dut.clock.step()
      dut.out.expect(1)

      dut.inp.poke(2)
      dut.clock.step()
      dut.out.expect(1)

      dut.inp.poke(4)
      dut.clock.step()
      dut.out.expect(1)

      dut.inp.poke(3)
      dut.clock.step()
      dut.out.expect(0)

      dut.inp.poke(5)
      dut.clock.step()
      dut.out.expect(0)

      dut.inp.poke(6)
      dut.clock.step()
      dut.out.expect(0)

      dut.inp.poke(7)
      dut.clock.step()
      dut.out.expect(1)

    }
  }
}
