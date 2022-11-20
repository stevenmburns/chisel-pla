// Example
package GCD

import org.scalatest.{ Matchers, FreeSpec, FlatSpec}

import chisel3._
import chisel3.iotesters._

class GenericTest( factory : () => GCDIfc) extends FlatSpec with Matchers {
  it should "meet all PeekPokeTester expectations" in {
    chisel3.iotesters.Driver( factory, "treadle") { c => new PeekPokeTester(c) {

      def run( u : BigInt, v : BigInt, z : BigInt) : Unit = {
        poke( c.io.ld, 1)
        poke( c.io.u, u)
        poke( c.io.v, v)
        step(1)
        poke( c.io.ld, 0)
        step(1)
        var count : Int = 0
        while( peek( c.io.done) == 0 && count < 200) {
//          println( s"Running: ${peek( c.io.z)}")
          step(1)
          count += 1
        }
//        println( s"Done: ${peek( c.io.z)}")
        expect( c.io.z, z)
      }

      run( 100, 25, 25)
      run( Fib(10), Fib(11), 1)
      run( Fib(20), Fib(21), 1)
      run( Fib(30), Fib(31), 1)
      run( 3*7*11*64,13*3*7*128, 3*7*64)

      rnd.setSeed(47)

      for { idx <- 0 until 1000} {
        val u = BigInt( 64, rnd)
        val v = BigInt( 64, rnd)
        val z = GCD(u , v)
        run( u, v, z)
      }
    }} should be (true)
  }
}

class BinaryGCDTest extends GenericTest( () => new BinaryGCD)
class BinaryGCDSimpleTest extends GenericTest( () => new BinaryGCDSimple)
class BinaryGCDNoBigShifterTest extends GenericTest( () => new BinaryGCDNoBigShifter)
class EuclidGCDTest extends GenericTest( () => new EuclidGCD)


class SoftwareTest extends FreeSpec with Matchers {
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

class SoftwareTest2 extends FreeSpec with Matchers {
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

class FibTest extends FreeSpec with Matchers {
  "Fib" - {
    "First 20 recursive, foldLeft Fibs equivalent" in {
      for ( n <- 0 until 20) {
        Fib(n) should be ( Fib.recursive(n))
      }
    }
  }
}
