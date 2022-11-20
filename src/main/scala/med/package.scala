import chisel3._
import chisel3.util._

package object med {
  def min( x : UInt, y : UInt) : UInt = {
    val result = WireInit( x)
    when( y < x) { result := y }
    result
  }

  def OptionallyInitializedShiftRegister[ T <: Data]( d : T, n : Int, o : Option[() => T] = None) : T = {
    if ( o.isEmpty) 
      ShiftRegister( d, n)
    else
      ShiftRegister( d, n, (o.get)(), true.B)
  }

/*
  def TappedShiftRegister[ T <: Data]( d : T, n : Int, o : Option[() => T] = None) : Vec[T] = {
    val rs = Vec.tabulate( n){ (i) => if ( o.isEmpty) Reg( d.cloneType) else RegInit( (o.get)())}
    Vec( rs.scanLeft( d){ case (x, y) => y := x; y})
  }
 */

  def TappedShiftRegister[ T <: Data]( d : T, n : Int, o : Option[() => T] = None) : Vec[T] = {
    val rs = IndexedSeq.tabulate( n){ (i) => if ( o.isEmpty) Reg( d.cloneType) else RegInit( (o.get)())}
    val rs0 =rs.scanLeft( d){ case (x, y) => y := x; y}
    val result = Wire( Vec( n+1, d.cloneType))
    (rs0 zip result).foreach{ case (x,y) => y := x}
    result
  }

/*
  def TappedShiftRegister[ T <: Data]( d : T, n : Int, o : Option[() => T] = None) : Vec[T] = {
    val result = Wire( Vec( n+1, d))
    result(0) := d
    for( i<-0 until n) {
      val r = if ( o.isEmpty) Reg( d.cloneType) else RegInit( (o.get)())
      r := result(i)
      result(i+1) := r
    }
    result
  }
 */


}
