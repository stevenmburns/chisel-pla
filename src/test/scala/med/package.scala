package med

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class TSRTester(factory : () => TappedShiftRegisterModule) extends AnyFreeSpec with ChiselScalatestTester {
  "Tapped Shift Register should work" in {
    test(factory()) { dut =>

      dut.io.inp.poke(47)

      dut.io.out(0).expect(47)

      dut.clock.step()
      dut.io.out(0).expect(47)
      dut.io.out(1).expect(47)

      dut.io.inp.poke(48)
      dut.io.out(0).expect(48)
      dut.clock.step()
      dut.io.out(0).expect(48)
      dut.io.out(1).expect(48)
      dut.io.out(2).expect(47)

      dut.io.inp.poke(49)
      dut.io.out(0).expect(49)
      dut.clock.step()
      dut.io.out(0).expect(49)
      dut.io.out(1).expect(49)
      dut.io.out(2).expect(48)
      dut.io.out(3).expect(47)

    }
  }
}

class TSRTest extends TSRTester(() => new TappedShiftRegisterModule)


