package parallel_prefix

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

object helpers {
  def largestPow2LessThan(x: BigInt): BigInt = {
    var y = BigInt(1)
    while (2 * y < x) {
      y *= 2
    }
    y
  }
}

class ParallelPrefix[T <: Data](gen: T, val n: Int) extends Module {
  val io = IO(new Bundle {
    val inp = Input(Vec(n, gen.cloneType))
    val out = Output(Vec(n, gen.cloneType))
  })
}

class Serial[T <: Data](gen: T, n: Int, op: (T, T) => T)
    extends ParallelPrefix(gen, n) {
  io.out(0) := io.inp(0)
  var prev = io.out(0)
  for {i <- 1 until n} {
    io.out(i) := op(prev, io.inp(i))
    prev = io.out(i)
  }
}

class Sklansky[T <: Data](gen: T, n: Int, op: (T, T) => T)
    extends ParallelPrefix(gen, n) {

  if (n == 1) {
    io.out(0) := io.inp(0)
  } else {
    val m = helpers.largestPow2LessThan(n).toInt

    val u = Module(new Sklansky(gen, m, op))
    val v = Module(new Sklansky(gen, n-m, op))

    for {i <- 0 until n} {
      if (i < m) {
        u.io.inp(i) := io.inp(i)
        io.out(i) := u.io.out(i)
      } else {
        v.io.inp(i-m) := io.inp(i)
        io.out(i) := op(io.out(m-1),v.io.out(i-m))
      }
    }
  }
}

class SklanskyFlat[T <: Data](gen: T, n: Int, op: (T, T) => T)
    extends ParallelPrefix(gen, n) {

  def aux(inp: Vec[T]): Vec[T] = {
    val n = inp.length
    if (n == 1) {
      inp
    } else {
      val m = helpers.largestPow2LessThan(n).toInt

      val inp0 = VecInit(IndexedSeq.tabulate(m){ i => inp(i) })
      val inp1 = VecInit(IndexedSeq.tabulate(n-m){ i => inp(i+m) })

      val out0 = aux(inp0)
      val out1 = aux(inp1)

      VecInit(IndexedSeq.tabulate(n){ i =>
        if (i < m) {
          out0(i)
        } else {
          op(out0(m-1),out1(i-m))
        }
      })
    }
  }

  io.out := aux(io.inp)
}

class KPG extends Bundle {
  val notk = Bool()
  val g = Bool()
}

object KPG {
  val k = (new KPG).Lit(_.notk -> false.B, _.g -> false.B)
  val p = (new KPG).Lit(_.notk -> true.B, _.g -> false.B)
  val g = (new KPG).Lit(_.notk -> true.B, _.g -> true.B)

  def op(a: KPG, b: KPG): KPG = {
    Mux(b === p, a, b)
  }
}

class PriorityEncoderIfc(val n: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(n.W))
    val z = Output(UInt(n.W))
  })
}

class PriorityEncoderSimple(n: Int) extends PriorityEncoderIfc(n) {
  io.z := PriorityEncoderOH(io.a)
}

class PriorityEncoder(n: Int, factory: () => ParallelPrefix[Bool]) extends PriorityEncoderIfc(n) {
  val u = Module(factory())
  u.io.inp := io.a.asBools
  val out = u.io.out.asUInt
  io.z := out & ~(out << 1)
}

class AdderIfc(val n: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(n.W))
    val b = Input(UInt(n.W))
    val z = Output(UInt(n.W))
  })
}

class AdderSimple(n: Int) extends AdderIfc(n) {
  io.z := io.a + io.b
}

class Adder(n: Int, factory: () => ParallelPrefix[KPG]) extends AdderIfc(n) {
  val u = Module(factory())

  u.io.inp := VecInit(IndexedSeq.tabulate(n){ i =>
    val x = WireInit(KPG.p)
    when        (!io.a(i) && !io.b(i)) {
      x := KPG.k
    } .elsewhen ( io.a(i) &&  io.b(i)) {
      x := KPG.g
    }
    x
  })

  io.z := VecInit(IndexedSeq.tabulate(n){ i =>
    if (i > 0) {
      (u.io.out(i-1) === KPG.g) ^ (u.io.inp(i) === KPG.p)
    } else {
      (u.io.inp(i) === KPG.p)
    }
  }).asUInt

}

object MainSerialOr extends App {
  println(getVerilogString(
    new Serial(Bool(), 5, {(x: Bool, y: Bool) => (x | y)})))
}

object MainSklanskyOr extends App {
  println(getVerilogString(
    new SklanskyFlat(Bool(), 5, {(x: Bool, y: Bool) => (x | y)})))
}

object MainSklanskyKPG extends App {
  println(getVerilogString(
    new SklanskyFlat(new KPG(), 5, KPG.op(_, _))))
}

object MainAdder extends App {
  println(getVerilogString(
    new Adder(5, () => new SklanskyFlat(new KPG(), 5, KPG.op(_, _)))))
}

