// See README.md for license details.

package alu

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._
import scala.util.Random

class AluSpecTester(factory : () => AluIfc) extends AnyFreeSpec with ChiselScalatestTester {

  "Alu should work" in {
    test(factory()) { dut =>
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

class AluRandomTester(factory : () => AluIfc) extends AnyFreeSpec with ChiselScalatestTester {

  "Alu should work" in {
    test(factory()) { dut =>

      def alu_aux(opcode: BigInt, a: Seq[BigInt], b: Seq[BigInt], mask: BigInt) : Seq[BigInt] = {
        val z = for {(aa, bb) <- a zip b} yield {
          if (opcode == 0) {
            (aa + bb) & mask
          } else if (opcode == 1) {
            (aa - bb) & mask
          } else if (opcode == 2) {
            aa & bb
          } else {
            aa | bb
          }
        }
        z
      }

      def alu_gold(mode: BigInt, opcode: BigInt, a: Seq[BigInt], b: Seq[BigInt]): Seq[BigInt] = {

        val m = (1<<16) - 1

        if (mode == 1) {
          alu_aux( opcode, a, b, (BigInt(1)<<16)-1)
        } else if ( mode == 2) {
          val aa = Seq((a(1)<<16) | a(0), (a(3)<<16) | a(2))
          val bb = Seq((b(1)<<16) | b(0), (b(3)<<16) | b(2))
          val zz = alu_aux( opcode, aa, bb, (BigInt(1)<<32)-1)
          Seq(zz(0)&m, (zz(0)>>16)&m, zz(1)&m, (zz(1)>>16)&m)
        } else {
          val aa = Seq((a(3)<< 48) | (a(2) << 32) | (a(1)<<16) | a(0))
          val bb = Seq((b(3)<< 48) | (b(2) << 32) | (b(1)<<16) | b(0))
          val zz = alu_aux( opcode, aa, bb, (BigInt(1)<<64)-1)
          Seq(zz(0)&m, (zz(0)>>16)&m, (zz(0)>>32)&m, (zz(0)>>48)&m)
        }
      }


      def set_and_check(mode: Int, opcode: Int,
        a : Seq[BigInt], b : Seq[BigInt]) : Unit = {

        val z = alu_gold(mode, opcode, a, b)

        println(s"mode=$mode opcode=$opcode a=$a b=$b expected=$z")

        dut.mode.poke(mode)
        dut.opcode.poke(opcode)
        for {i <- 0 until 4} {
          dut.a(i).poke(a(i))
          dut.b(i).poke(b(i))
        }
        dut.clock.step()
        for {i <- 0 until 4} {
          dut.z(i).expect(z(i))
        }
      }



      val m1 = (1<<16)-1
      set_and_check(1, 0, Seq(0,1,2,3), Seq(3,2,1,0))
      set_and_check(1, 1, Seq(0,1,2,3), Seq(3,2,1,0))
      set_and_check(1, 2, Seq(0,1,2,3), Seq(3,2,1,0))
      set_and_check(1, 3, Seq(0,1,2,3), Seq(3,2,1,0))
      set_and_check(1, 0, Seq(m1,m1,m1,m1), Seq(1,0,0,0))
      set_and_check(2, 0, Seq(m1,m1,m1,m1), Seq(1,0,0,0))
      set_and_check(4, 0, Seq(m1,m1,m1,m1), Seq(1,0,0,0))
      set_and_check(1, 1, Seq(1,0,0,0), Seq(1,1,1,1))
      set_and_check(2, 1, Seq(1,0,0,0), Seq(1,0,1,0))
      set_and_check(4, 1, Seq(1,0,0,0), Seq(1,0,0,0))

      val rnd = new Random()

      for {i <- 0 until 1000} {
        val mode = Seq(1,2,4)(rnd.nextInt(3))
        val opcode = Seq(0,1,2,3)(rnd.nextInt(4))
        val a = for {j <- 0 until 4} yield BigInt(16, rnd)
        val b = for {j <- 0 until 4} yield BigInt(16, rnd)

        set_and_check(mode, opcode, a, b)
      }
    }
  }
}


//class AluSpecTest extends AluSpecTester(() => new Alu) 
//class AluMMXSpecTest extends AluSpecTester(() => new AluMMX)
class AluMMXRandomTest extends AluRandomTester(() => new AluMMX)
