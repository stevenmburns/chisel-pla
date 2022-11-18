// See README.md for license details.

package alu

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._

class AluSpec extends AnyFreeSpec with ChiselScalatestTester {

  "Alu should work" in {
    test(new Alu) { dut =>
      dut.mode.poke(1)

      dut.opcode.poke(0)
      dut.a(0).poke(0)
      dut.a(1).poke(1)
      dut.a(2).poke(2)
      dut.a(3).poke(3)
      dut.b(0).poke(3)
      dut.b(1).poke(2)
      dut.b(2).poke(1)
      dut.b(3).poke(0)

      dut.clock.step()
      dut.z(0).expect(3)
      dut.z(1).expect(3)
      dut.z(2).expect(3)
      dut.z(3).expect(3)

      dut.opcode.poke(1)
      dut.clock.step()
      dut.z(0).expect((1<<16) - 3)
      dut.z(1).expect((1<<16) - 1)
      dut.z(2).expect(1)
      dut.z(3).expect(3)

      dut.opcode.poke(2)
      dut.clock.step()
      dut.z(0).expect(0)
      dut.z(1).expect(0)
      dut.z(2).expect(0)
      dut.z(3).expect(0)

      dut.opcode.poke(3)
      dut.clock.step()
      dut.z(0).expect(3)
      dut.z(1).expect(3)
      dut.z(2).expect(3)
      dut.z(3).expect(3)

      dut.mode.poke(1)
      dut.opcode.poke(0)

      dut.a(0).poke((1<<16)-1)
      dut.a(1).poke((1<<16)-1)
      dut.a(2).poke((1<<16)-1)
      dut.a(3).poke((1<<16)-1)
      dut.b(0).poke(1)
      dut.b(1).poke(0)
      dut.b(2).poke(0)
      dut.b(3).poke(0)
      dut.clock.step()
      dut.z(0).expect(0)
      dut.z(1).expect((1<<16)-1)
      dut.z(2).expect((1<<16)-1)
      dut.z(3).expect((1<<16)-1)

      dut.mode.poke(2)
      dut.opcode.poke(0)

      dut.a(0).poke((1<<16)-1)
      dut.a(1).poke((1<<16)-1)
      dut.a(2).poke((1<<16)-1)
      dut.a(3).poke((1<<16)-1)
      dut.b(0).poke(1)
      dut.b(1).poke(0)
      dut.b(2).poke(0)
      dut.b(3).poke(0)
      dut.clock.step()
      dut.z(0).expect(0)
      dut.z(1).expect(0)
      dut.z(2).expect((1<<16)-1)
      dut.z(3).expect((1<<16)-1)

      dut.mode.poke(4)
      dut.opcode.poke(0)

      dut.a(0).poke((1<<16)-1)
      dut.a(1).poke((1<<16)-1)
      dut.a(2).poke((1<<16)-1)
      dut.a(3).poke((1<<16)-1)
      dut.b(0).poke(1)
      dut.b(1).poke(0)
      dut.b(2).poke(0)
      dut.b(3).poke(0)
      dut.clock.step()
      dut.z(0).expect(0)
      dut.z(1).expect(0)
      dut.z(2).expect(0)
      dut.z(3).expect(0)
      dut.mode.poke(1)
      dut.opcode.poke(0)

      dut.mode.poke(1)
      dut.opcode.poke(1)

      dut.a(0).poke(1)
      dut.a(1).poke(0)
      dut.a(2).poke(0)
      dut.a(3).poke(0)
      dut.b(0).poke(1)
      dut.b(1).poke(1)
      dut.b(2).poke(1)
      dut.b(3).poke(1)
      dut.clock.step()
      dut.z(0).expect(0)
      dut.z(1).expect((1<<16)-1)
      dut.z(2).expect((1<<16)-1)
      dut.z(3).expect((1<<16)-1)

      dut.mode.poke(2)
      dut.opcode.poke(1)

      dut.a(0).poke(1)
      dut.a(1).poke(0)
      dut.a(2).poke(0)
      dut.a(3).poke(0)
      dut.b(0).poke(1)
      dut.b(1).poke(0)
      dut.b(2).poke(1)
      dut.b(3).poke(0)
      dut.clock.step()
      dut.z(0).expect(0)
      dut.z(1).expect(0)
      dut.z(2).expect((1<<16)-1)
      dut.z(3).expect((1<<16)-1)

      dut.mode.poke(4)
      dut.opcode.poke(1)

      dut.a(0).poke(1)
      dut.a(1).poke(0)
      dut.a(2).poke(0)
      dut.a(3).poke(0)
      dut.b(0).poke(1)
      dut.b(1).poke(0)
      dut.b(2).poke(0)
      dut.b(3).poke(0)
      dut.clock.step()
      dut.z(0).expect(0)
      dut.z(1).expect(0)
      dut.z(2).expect(0)
      dut.z(3).expect(0)
    }
  }
}
