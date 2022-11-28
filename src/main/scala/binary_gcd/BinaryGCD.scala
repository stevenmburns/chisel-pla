// Example
package GCD

import chisel3._

class GCDIfc extends Module {
  val io = IO( new Bundle {
    val ld = Input( Bool())
    val u = Input( UInt(64.W))
    val v = Input( UInt(64.W))
    val z = Output( UInt(64.W))
    val done = Output( Bool())
  })
}

class EuclidGCD extends GCDIfc {

  val u = Reg( io.u.cloneType)
  val v = Reg( io.v.cloneType)

  when( io.ld) {
    u := io.u
    v := io.v
  } .elsewhen ( v =/= 0.U) {
    u := v
    v := u % v
  }

  io.z := u
  io.done := v === 0.U
}

class BinaryGCD extends GCDIfc {

  val u = Reg( io.u.cloneType)
  val v = Reg( io.v.cloneType)
  val k = Reg( UInt((io.u.cloneType.getWidth-1).U.getWidth.W))

  when( io.ld) {
    u := io.u
    v := io.v
    k := 0.U
  } .elsewhen ( io.done) {
  } .elsewhen ( !u(0) && !v(0)) {
    val uv = WireInit( u | v)

    when ( !uv(0) && uv(1)) {
        u := u >> 1
        v := v >> 1
        k := k + 1.U
    } .elsewhen ( !uv(1) && uv(2)) {
        u := u >> 2
        v := v >> 2
        k := k + 2.U
    } .otherwise {
        u := u >> 3
        v := v >> 3
        k := k + 3.U
    }
  } .otherwise {
    val u0 = WireInit( u)
    val v0 = WireInit( v)

    when ( u(0) && ( !v(0) || v > u)) {
      u0 := v
      v0 := u
    }

    v := v0
    when ( !u0(0)) {
      when ( !u0(0) && u0(1)) {
        u := u0 >> 1
      } .elsewhen ( !u0(1) && u0(2)) {
        u := u0 >> 2
      } .otherwise {
        u := u0 >> 3
      }
    } .otherwise {
      u := (u0-v0) >> 1
    }
  }

  io.z := u << k
  io.done := u === v || v === 0.U
}

class BinaryGCDSimple extends GCDIfc {

  def isEven( u : UInt) : Bool = !u(0)
  def shiftRight( u : UInt) : UInt = u >> 1

  val u = Reg( io.u.cloneType)
  val v = Reg( io.v.cloneType)
  val k = Reg( UInt((io.u.cloneType.getWidth-1).U.getWidth.W))

  when( io.ld) {
    u := io.u
    v := io.v
    k := 0.U
  } .elsewhen ( io.done) {
  } .elsewhen ( isEven( u) && isEven( v)) {
    u := u >> 1
    v := v >> 1
    k := k + 1.U
  } .otherwise {
    val u0 = WireInit( u)
    val v0 = WireInit( v)

    when ( !isEven( u) && ( isEven( v) || v > u)) {
      u0 := v
      v0 := u
    }

    v := v0
    when ( isEven( u0)) {
      u := shiftRight( u0)
    } .otherwise {
      u := shiftRight( u0-v0)
    }
  }

  io.z := u << k
  io.done := u === v || v === 0.U
}

class BinaryGCDNoBigShifter extends GCDIfc {

  def isEven( u : UInt, m : UInt) : Bool = (u & m) === 0.U
  def shiftRight( u : UInt, m : UInt) : UInt = ( u >> 1) & ~( m - 1.U)

  val u = Reg( io.u.cloneType)
  val v = Reg( io.v.cloneType)
  val m = Reg( io.u.cloneType)

  when( io.ld) {
    u := io.u
    v := io.v
    m := 1.U
  } .elsewhen ( io.done) {
  } .elsewhen ( isEven( u, m) && isEven( v, m)) {
    m := m << 1
  } .otherwise {
    val u0 = WireInit( u)
    val v0 = WireInit( v)

    when ( !isEven( u, m) && ( isEven( v, m) || v > u)) {
      u0 := v
      v0 := u
    }

    v := v0
    when ( isEven( u0, m)) {
      u := shiftRight( u0, m)
    } .otherwise {
      u := shiftRight( u0-v0, m)
    }
  }

  io.z := u
  io.done := u === v || v === 0.U
}


object GCD {

  def isEven( u : BigInt) : Boolean = (u & 1) == 0

  def trailingZeroes( u : BigInt) : Int = {
    @annotation.tailrec
    def aux( u : BigInt, k : Int) : Int = if ( isEven( u)) aux( u>>1, k+1) else k
    aux( u, 0)
  }


  def apply( u : BigInt, v : BigInt) : BigInt = {

    @annotation.tailrec
    def aux( u : BigInt, v : BigInt, k : Int) : BigInt = {
//      println( s"aux: ${u} ${v} ${k}")
      if ( u == v || v == 0) u<<k
      else if ( isEven( u) && isEven( v)) {
        val kk = trailingZeroes( u | v)
        aux( u >> kk, v >> kk, k+kk)
      } else {
        val (u0,v0) = if ( !isEven( u) && (isEven( v) || v > u)) (v,u) else (u,v)
        assert( isEven(u0) || u0 >= v0)
        if ( isEven( u0)) {
          val kk = trailingZeroes( u0)
          aux( u0 >> kk, v0, k)
/*
        } else if ( ((u0 ^ v0) & 2) == 2) aux( (u0+v0)>>2, v0, k)
        else aux( (u0-v0)>>2, v0, k)
 */
        } else aux( (u0-v0)>>1, v0, k)
      }
    }

    aux( u, v, 0)
  }

  def apply2( u : BigInt, v : BigInt) : BigInt = {

    def isEven( u : BigInt, m : BigInt) : Boolean = (u & m) != m
    def shiftRight( u : BigInt, m : BigInt) : BigInt = ( u >> 1) & ~( m - 1)

    @annotation.tailrec
    def aux( u : BigInt, v : BigInt, m : BigInt) : BigInt = {
//      println( s"aux: ${u} ${v} ${m}")
      if ( u == v || v == 0) u
      else if ( isEven( u, m) && isEven( v, m)) {
        aux( u, v, m<<1)
      } else {
        val (u0,v0) = if ( !isEven( u, m) && (isEven( v, m) || v > u)) (v,u) else (u,v)
        assert( isEven(u0, m) || u0 >= v0)
        if ( isEven( u0, m)) aux( shiftRight( u0, m), v0, m)
        else aux( shiftRight( u0-v0, m), v0, m)
      }
    }

    aux( u, v, 1)
  }

  @annotation.tailrec
  def euclid( u : BigInt, v : BigInt) : BigInt = {
//    println( s"euclid: ${u} ${v}")
    if ( v == 0) u else euclid( v, u % v)
  }
}

object Fib {
  def recursive( n: Int) : BigInt = if ( n < 2) n else recursive( n-2) + recursive( n-1)
  def apply( n: Int) : BigInt = 
    ((0 until n).foldLeft( (BigInt(0),BigInt(1))){ case ((f0,f1),_) => (f1,f0+f1)})._1
}

object BinaryGCD extends App {
  println(getVerilogString(new BinaryGCD))
}

object BinaryGCDNoBigShifter extends App {
  println(getVerilogString(new BinaryGCDNoBigShifter))
}

object BinaryGCDSimple extends App {
  println(getVerilogString(new BinaryGCDSimple))
}

object EuclidGCD extends App {
  println(getVerilogString(new EuclidGCD))
}
