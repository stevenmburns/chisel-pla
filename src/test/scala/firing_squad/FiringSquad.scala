package firing_squad

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class EightState119Test extends AnyFreeSpec with ChiselScalatestTester {

  val fs = EightState119

  "EightState119 should work" in {
    test(new FiringSquadArray( () => new FiringSquadSite(fs), 22)) { dut =>

      val inverseMap = for ( (k,v) <- fs.m) yield v -> k

      val n = dut.n
      val gold = IndexedSeq.fill( n)(fs.f.U)

      dut.io.start.poke(false.B)
      dut.clock.step()
      dut.io.start.poke(true.B)

      for( i <- 0 until 2*n-1) {
        dut.clock.step()
        val s =  dut.io.states.peek()

        val sStr = (for( u <- s) yield inverseMap(u.litValue.toInt)).mkString( "")
        println( f"$i%03d: $sStr")

        //assert (s == gold) === (i == 2*n-2)
      }
    }
  }
}

class SixStateTest extends AnyFreeSpec with ChiselScalatestTester {

  val fs = SixState

  "SixStatee should work" in {
    test(new FiringSquadArray( () => new FiringSquadSite(fs), 22)) { dut =>

      val inverseMap = for ( (k,v) <- fs.m) yield v -> k

      val n = dut.n
      val gold = IndexedSeq.fill( n)(fs.f.U)

      dut.io.start.poke(false.B)
      dut.clock.step()
      dut.io.start.poke(true.B)

      for( i <- 0 until 2*n-1) {
        dut.clock.step()
        val s =  dut.io.states.peek()

        val sStr = (for( u <- s) yield inverseMap(u.litValue.toInt)).mkString( "")
        println( f"$i%03d: $sStr")

        //assert (s == gold) === (i == 2*n-2)
      }
    }
  }
}

// sbt 'testOnly firing_squad.SixStateTest'

// sbt 'testOnly firing_squad.EightState119Test'
