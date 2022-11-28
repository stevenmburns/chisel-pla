package med

import chisel3._
import chisel3.util._

class TappedShiftRegisterModule extends Module {
  val io = IO( new Bundle {
    val inp = Input( UInt(8.W))
    val out = Output( Vec( 5, UInt(8.W)))
  })

  io.out := TappedShiftRegister( io.inp, 4, None)
}

class CellIfc[TD <: Data, TS <: Data]( protoD : TD, protoS : TS)( pos : Option[(Int,Int)]) extends Module {
  val io = IO( new Bundle {
    val col = Input( protoD)
    val row = Input( protoD)
    val up = Input( protoS)


    val lf = Input( protoS)
    val dg = Input( protoS)
    val out = Output( protoS)
  })
}

class CellFixture[TD <: Data, TS <: Data]( factory : () => CellIfc[TD,TS], protoD : TD, protoS : TS) extends CellIfc[TD,TS]( protoD, protoS)( None) {

  def restrict( x : TS) : TS = {
    val y = WireInit( x)
    when ( 255.U === x.asUInt /*|| 254.U === x.asUInt*/) {
      y := 0.U
    }
    y
  }

  val m = Module( factory())

  m.io.col := io.col
  m.io.row := io.row

  m.io.up  := io.up
  m.io.lf  := io.lf
  m.io.dg  := io.dg
  m.io.up  := restrict( io.up)
  m.io.lf  := restrict( io.lf)
//  m.io.dg  := restrict( io.dg)

  io.out := m.io.out


}


object Cell {
  def boundary( i : Int, j : Int) : UInt = {
    if ( j == -1) 
      (i+1).U
    else if ( i == -1)
      (j+1).U
    else 
      throw new Exception( s"Expecting at least one coord ($i,$j) to be minus 1")
  }
  def initD() : UInt = 0.U
  def initS() : UInt = 0.U
}

class Cell[TD <: UInt, TS <: UInt]( protoD : TD, protoS : TS)( pos : Option[(Int,Int)] = None) extends
  CellIfc[TD,TS]( protoD, protoS)( pos) {

  io.out := min( min( io.up, io.lf) + 1.U,
                 Mux(io.col =/= io.row, io.dg + 2.U, io.dg))

}

/*
class Cell2[TD <: UInt, TS <: UInt]( protoD : TD, protoS : TS)( pos : Option[(Int,Int)] = None) extends
  CellIfc[TD,TS]( protoD, protoS)( pos) {


  val dg_score = WireInit( io.dg)
  when ( io.col =/= io.row) { dg_score := io.dg + 2.U}

  io.out := min( min( io.up + 1.U, io.lf + 1.U), dg_score)

}
 */

class MockData extends Bundle {
  val ts = SInt(32.W)
  val i = SInt(9.W)
  val j = SInt(9.W)
}

class MockScore extends Bundle {
  val ts = SInt(32.W)
  val i = SInt(9.W)
  val j = SInt(9.W)
}

class MockCell( protoD : MockData, protoS : MockScore)( pos : Option[(Int,Int)]) extends
    CellIfc( protoD, protoS)( pos) {

  assert( io.row.j === (-1).S)
  assert( io.col.i === (-1).S)

  assert( io.row.ts === io.col.ts || io.row.ts === (-1).S || io.col.ts === (-1).S )

  io.out.ts := io.row.ts

  io.out.i := io.row.i
  io.out.j := io.col.j


  assert( io.dg.ts === io.out.ts || io.dg.ts === (-1).S || io.out.ts === (-1).S)
  assert( io.up.ts === io.out.ts || io.up.ts === (-1).S || io.out.ts === (-1).S)
  assert( io.lf.ts === io.out.ts || io.lf.ts === (-1).S || io.out.ts === (-1).S)


  if ( !pos.isEmpty) {
    assert( io.out.i === pos.get._1.S || io.out.i === (-1).S)
    assert( io.out.j === pos.get._2.S || io.out.j === (-1).S)
  }

  assert( io.out.i === io.lf.i       || io.out.i === (-1).S)
  assert( io.out.j === io.lf.j + 1.S || io.out.j === (-1).S)

  assert( io.out.i === io.up.i + 1.S || io.out.i === (-1).S)
  assert( io.out.j === io.up.j       || io.out.j === (-1).S)

  assert( io.out.i === io.dg.i + 1.S || io.out.i === (-1).S)
  assert( io.out.j === io.dg.j + 1.S || io.out.j === (-1).S)

  printf( s"row: ${pos.get._1} col: ${pos.get._2} out.ts: %d up.ts: %d lf.ts: %d dg.ts: %d row.ts: %d col.ts: %d\n",
    io.out.ts,
    io.up.ts, io.lf.ts, io.dg.ts,
    io.row.ts, io.col.ts)
 
}

object MockCell {
  def boundary( i : Int, j : Int) : MockScore = {
    val ms = Wire( new MockScore)
    ms.ts := (-1).S
    ms.i := i.S
    ms.j := j.S
    if ( i == -1 || j == -1)
      ms
    else
      throw new Exception( "Expected one coord to be -1")
    ms
  }
  def initD() : MockData = {
    val i = Wire( new MockData)
    i.ts := (-1).S
    i.i := (-1).S
    i.j := (-1).S
    i
  }
  def initS() : MockScore = {
    val i = Wire( new MockScore)
    i.ts := (-1).S
    i.i := (-1).S
    i.j := (-1).S
    i
  }
}

class SimpleMockData extends Bundle {
  val ts = SInt(32.W)
}

class SimpleMockScore extends Bundle {
  val ts = SInt(32.W)
}

class SimpleMockCell( protoD : SimpleMockData, protoS : SimpleMockScore)( pos : Option[(Int,Int)]) extends
    CellIfc( protoD, protoS)( pos) {

  assert( io.row.ts === io.col.ts || io.row.ts === (-1).S || io.col.ts === (-1).S )

  io.out.ts := io.row.ts

  assert( io.dg.ts === io.out.ts || io.dg.ts === (-1).S || io.out.ts === (-1).S)
  assert( io.up.ts === io.out.ts || io.up.ts === (-1).S || io.out.ts === (-1).S)
  assert( io.lf.ts === io.out.ts || io.lf.ts === (-1).S || io.out.ts === (-1).S)

  if ( !pos.isEmpty) {
    printf( s"row: ${pos.get._1} col: ${pos.get._2} out.ts: %d up.ts: %d lf.ts: %d dg.ts: %d row.ts: %d col.ts: %d\n",
      io.out.ts,
      io.up.ts, io.lf.ts, io.dg.ts,
      io.row.ts, io.col.ts)
  }
 
}

object SimpleMockCell {
  def boundary( i : Int, j : Int) : SimpleMockScore = {
    val ms = Wire( new SimpleMockScore)
    ms.ts := (-1).S
    ms
  }
  def initD() : SimpleMockData = {
    val i = Wire( new SimpleMockData)
    i.ts := (-1).S
    i
  }
  def initS() : SimpleMockScore = {
    val i = Wire( new SimpleMockScore)
    i.ts := (-1).S
    i
  }
}

class CellArrayIfc[TD <: Data, TS <: Data]( val M : Int, val N : Int, protoD : TD, protoS : TS) extends Module {

  val io = IO( new Bundle{
    val rows = Input( Vec( M, protoD))
    val cols = Input( Vec( N, protoD))
    val out = Output( protoS)
  })

  val delay : Int = 0

}

class CellArray[TD <: Data, TS <: Data]
  ( M : Int, N : Int,
    protoD : TD, protoS : TS,
    factory : (Int,Int) => CellIfc[TD,TS],
    boundary : (Int,Int) => TS)
    extends CellArrayIfc( M, N, protoD, protoS) {

//  override val delay : Int = M+N-2
  override val delay : Int = 0

  val cells = IndexedSeq.tabulate( M, N){ (i,j) => Module( factory(i,j))}

  for { i <- 0 until M
        j <- 0 until N} {

    cells(i)(j).io.up := { if ( i>0) cells(i-1)(j).io.out else boundary( -1, j) }
    cells(i)(j).io.lf := { if ( j>0) cells(i)(j-1).io.out else boundary( i, -1) }
    cells(i)(j).io.dg := {
      if ( i>0 && j>0)
        cells(i-1)(j-1).io.out
      else if ( i == 0)
        boundary( -1, j-1)
      else /* j == 0 */
        boundary( i-1, -1)
    }

    cells(i)(j).io.row := io.rows(i)
    cells(i)(j).io.col := io.cols(j)
  }

/*
  for { i <- 0 until M
        j <- 0 until N} {
    printf( s"cells($i)($j).io.up:  %d\n", cells(i)(j).io.up.asUInt)
    printf( s"cells($i)($j).io.lf:  %d\n", cells(i)(j).io.lf.asUInt)
    printf( s"cells($i)($j).io.dg:  %d\n", cells(i)(j).io.dg.asUInt)
    printf( s"cells($i)($j).io.out: %d\n", cells(i)(j).io.out.asUInt)
  }
 */

  io.out := ShiftRegister( cells(M-1)(N-1).io.out, delay)

}

class CellArrayRetimed[TD <: Data, TS <: Data]
  ( M : Int, N : Int,
    protoD : TD, protoS : TS,
    factory : (Int,Int) => CellIfc[TD,TS],
    boundary : (Int,Int) => TS,
    initD : Option[() => TD] = None,
    initS : Option[() => TS] = None) 
    extends CellArrayIfc( M, N, protoD, protoS) {

  override val delay : Int = M + N - 2

  val cells = IndexedSeq.tabulate( M, N){ (i,j) => Module( factory(i,j))}
  val outs = IndexedSeq.tabulate( M, N){ (i,j) => Wire( protoS)}

  val tappedRows = IndexedSeq.tabulate( M){ (i) => TappedShiftRegister[TD]( io.rows(i), i + N, initD)}
  val tappedCols = IndexedSeq.tabulate( N){ (j) => TappedShiftRegister[TD]( io.cols(j), j + M, initD)}

  for { i <- 0 until M
        j <- 0 until N} {

    outs(i)(j) := OptionallyInitializedShiftRegister( cells(i)(j).io.out, 1, initS)

    cells(i)(j).io.up := { if ( i>0) outs(i-1)(j) else boundary( -1, j) }
    cells(i)(j).io.lf := { if ( j>0) outs(i)(j-1) else boundary( i, -1) }
    cells(i)(j).io.dg := {
      if ( i>0 && j>0)
        OptionallyInitializedShiftRegister( outs(i-1)(j-1), 1, initS)
      else if ( i == 0)
        boundary( -1, j-1)
      else /* j == 0 */
        boundary( i-1, -1)
    }

//    cells(i)(j).io.row := OptionallyInitializedShiftRegister( io.rows(i), i+j, initD)
//    cells(i)(j).io.col := OptionallyInitializedShiftRegister( io.cols(j), i+j, initD)

    cells(i)(j).io.row := tappedRows(i)( i+j)
    cells(i)(j).io.col := tappedCols(j)( i+j)

  }

  io.out := ShiftRegister( cells(M-1)(N-1).io.out, 0)

}

object CellArray33 extends App {
  val (protoD,protoS) = (UInt(2.W),UInt(8.W))
  println(getVerilogString(new CellArray( 3, 3, protoD, protoS, (i,j) => new Cell( protoD, protoS)( Some((i,j))), Cell.boundary)))
}

object CellArray44 extends App {
  val (protoD,protoS) = (UInt(2.W),UInt(8.W))
  println(getVerilogString(new CellArray( 4, 4, protoD, protoS, (i,j) => new Cell( protoD, protoS)( Some((i,j))), Cell.boundary)))
}

object CellArrayRetimed44 extends App {
  val (protoD,protoS) = (UInt(2.W),UInt(8.W))
  println(getVerilogString(new CellArrayRetimed( 4, 4, protoD, protoS, (i,j) => new Cell( protoD, protoS)( Some((i,j))), Cell.boundary)))
}

object CellArray55 extends App {
  val (protoD,protoS) = (UInt(2.W),UInt(8.W))
  println(getVerilogString(new CellArray( 5, 5, protoD, protoS, (i,j) => new Cell( protoD, protoS)( Some((i, j))), Cell.boundary)))
}

object CellArray1616 extends App {
  val (protoD,protoS) = (UInt(2.W),UInt(8.W))
  println(getVerilogString(new CellArray( 16, 16, protoD, protoS, (i,j) => new Cell( protoD, protoS)( Some((i, j))), Cell.boundary)))
}

object CellArrayRetimed1616 extends App {
  val (protoD,protoS) = (UInt(2.W),UInt(8.W))
  println(getVerilogString(new CellArrayRetimed( 16, 16, protoD, protoS, (i,j) => new Cell( protoD, protoS)( Some((i, j))), Cell.boundary)))
}

