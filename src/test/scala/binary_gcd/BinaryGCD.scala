// Example
package GCD

import chisel3._
import chiseltest._
import org.scalatest.matchers.should.Matchers
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._
import scala.util.Random

class GenericTester( factory : () => GCDIfc) extends AnyFreeSpec with ChiselScalatestTester {
  "GCD should work" in {
    test(factory()) { dut =>

      def run( u : BigInt, v : BigInt, z : BigInt) : Unit = {
        dut.io.ld.poke(1)
        dut.io.u.poke(u)
        dut.io.v.poke(v)
        dut.clock.step()
        dut.io.ld.poke(0)
        dut.clock.step()
        var count : Int = 0
        while( dut.io.done.peek() == 0 && count < 200) {
//          println( s"Running: ${peek( c.io.z)}")
          dut.clock.step()
          count += 1
        }
//        println( s"Done: ${peek( c.io.z)}")
        dut.io.z.expect(z)
      }

      run( 100, 25, 25)
      run( Fib(10), Fib(11), 1)
      run( Fib(20), Fib(21), 1)
      run( Fib(30), Fib(31), 1)
      run( 3*7*11*64,13*3*7*128, 3*7*64)

      val rnd = new Random()
      rnd.setSeed(47)

      for { idx <- 0 until 1000} {
        val u = BigInt( 64, rnd)
        val v = BigInt( 64, rnd)
        val z = GCD(u , v)
        run( u, v, z)
      }
    }
  }
}

class BinaryGCDTest extends GenericTester( () => new BinaryGCD)
class BinaryGCDSimpleTest extends GenericTester( () => new BinaryGCDSimple)
class BinaryGCDNoBigShifterTest extends GenericTester( () => new BinaryGCDNoBigShifter)
class EuclidGCDTest extends GenericTester( () => new EuclidGCD)


class SoftwareTest extends AnyFreeSpec with Matchers {
  "GCD" - {
    "GCD with second multiple of first works" in {
      val (u,v) = (25,100)
      GCD( u, v) should be ( GCD.euclid( u, v))
    }
    "GCD with custom args works" in {
      val (u,v) = (3*7*11*64,13*3*7*128)
      GCD( u, v) should be ( GCD.euclid( u, v))
    }
    "GCD with arb args works" in {
      val (u,v) = (2354346,4566334)
      GCD( u, v) should be ( GCD.euclid( u, v))
    }
  }
}

class SoftwareTest2 extends AnyFreeSpec with Matchers {
  "GCD2" - {
    "GCD with second multiple of first works" in {
      val (u,v) = (25,100)
      GCD.apply2( u, v) should be ( GCD.euclid( u, v))
    }
    "GCD with first multiple of second works" in {
      val (u,v) = (100,25)
      GCD.apply2( u, v) should be ( GCD.euclid( u, v))
    }
    "GCD with custom args works" in {
      val (u,v) = (3*7*11*64,13*3*7*128)
      GCD.apply2( u, v) should be ( GCD.euclid( u, v))
    }
    "GCD with arb args works" in {
      val (u,v) = (2354346,4566334)
      GCD.apply2( u, v) should be ( GCD.euclid( u, v))
    }
  }
}

class FibTest extends AnyFreeSpec with Matchers {
  "Fib" - {
    "First 20 recursive, foldLeft Fibs equivalent" in {
      for ( n <- 0 until 20) {
        Fib(n) should be ( Fib.recursive(n))
      }
    }
  }
}
