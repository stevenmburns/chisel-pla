package firing_squad

import chisel3._
import chisel3.util._

trait FS {

  def boundary : Int
  def general : Int
  def init : Int
  def fire : Int
  def dont_care : Int

  def productionRules : Map[Int,Map[(Int,Int),Int]]

  def m : Map[Char,Int]

  def processTable( s : String) : Map[(Int,Int),Int] = {
    val mm = collection.mutable.Map[(Int,Int),Int]()

    val lst = s.split('\n')
    assert( lst.head == "")
    val header = lst.tail.head
    assert( header(0) == ' ')
    val headerLst = header.toList.tail
    val n = headerLst.length

//    println( s"${headerLst}")

    assert( lst.tail.tail.length == n)

    for ( (line,rowHeading) <- lst.tail.tail zip headerLst) {
      assert( line.length <= n+1)
      val rowStr = line.padTo( n+1, ' ')
      assert( rowStr.toList.head == rowHeading)
      val row = rowStr.toList.tail
      assert( row.length == n)

//      println( s"${row}")

      for( (x,colHeading) <- row zip headerLst) {
        if ( x != ' ') {
          mm( ( m(rowHeading), m(colHeading))) = m(x)
        }
      }
    }

    mm.toMap
  }


}

object EightState119 extends FS {
  val boundary :: dot :: dash :: lt :: gt :: circle :: splot :: g :: f :: dont_care :: Nil = (0 until 10).toList

  def general = g
  def init = dash
  def fire = f

  val m = Map(
    '*' -> boundary,
    '.' -> dot,
    '-' -> dash,
    '<' -> lt,
    '>' -> gt,
    '⚪' -> circle,
    '⚫' -> splot,
    'G' -> g,
    'F' -> f,
    ' ' -> dont_care
  )

  val tbl_dot =
"""
 *.-<>⚪⚫G
*
. . <>..>
-
< . <>..>
>
⚪ . <>..>
⚫ . <⚫..⚫
G   ⚫ ..G
"""

  val tbl_dash = 
"""
 *.-<>⚪⚫G
*  -
.
-- - ---
<G < <<⚫
>  > >>>⚫
⚪  - ----
⚫  - ----
GG < <<⚫G
"""

  val tbl_lt = 
"""
 *.-<>⚪⚫G
*
. ..  .  
-
<
>
⚪ .
⚫ ⚪
G  .  .
"""

  val tbl_gt = 
"""
 *.-<>⚪⚫G
*
.  -    -
-  -  -⚪
<
>
⚪  -    -
⚫
G
"""

  val tbl_circle = 
"""
 *.-<>⚪⚫G
*
. ⚪ ⚫G
-  ⚪ ⚪
< ⚪G⚫G
>  ⚫ ⚫
⚪
⚫
G
"""

  val tbl_splot = 
"""
 *.-<>⚪⚫G
*
. ⚫ <  G
-  ⚫    ⚫
<
>  >    -
⚪
⚫  G    G
G ⚫ .  G
"""

  val tbl_g = 
"""
 *.-<>⚪⚫G
* GGG  GF
.G G    G
-GG     G
<
>G  G   G
⚪
⚫G     GG
GFGGG  GF
"""

  val productionRules = Map(
    dot-> processTable( tbl_dot),
    dash -> processTable( tbl_dash),
    lt -> processTable( tbl_lt),
    gt -> processTable( tbl_gt),
    circle -> processTable( tbl_circle),
    splot -> processTable( tbl_splot),
    g -> processTable( tbl_g)
  )

}


object EightState119simple extends FS {
  val boundary :: dot :: dash :: lt :: gt :: circle :: splot :: g :: f :: dont_care :: Nil = (0 until 10).toList

  def general = g
  def init = dash
  def fire = f

  val m = Map(
    '*' -> boundary,
    '.' -> dot,
    '-' -> dash,
    '<' -> lt,
    '>' -> gt,
    '⚪' -> circle,
    '⚫' -> splot,
    'G' -> g,
    'F' -> f,
    ' ' -> dont_care
  )

  val tbl_dot =
"""
 *.-<>⚪⚫G
*
.   <>  >
-
<   <>  >
>
⚪   <>  >
⚫   <⚫  ⚫
G   ⚫   G
"""

  val tbl_dash = 
"""
 *.-<>⚪⚫G
*  -
.
-
<G < <<⚫
>  > >>>⚫
⚪
⚫
GG < <<⚫G
"""

  val tbl_lt = 
"""
 *.-<>⚪⚫G
*
. ..  .  
-
<
>
⚪ .
⚫ ⚪
G  .  .
"""

  val tbl_gt = 
"""
 *.-<>⚪⚫G
*
.  -    -
-  -  -⚪
<
>
⚪  -    -
⚫
G
"""

  val tbl_circle = 
"""
 *.-<>⚪⚫G
*
.   ⚫G
-
<  G⚫G
>  ⚫ ⚫
⚪
⚫
G
"""

  val tbl_splot = 
"""
 *.-<>⚪⚫G
*
. ⚫ <  G
-
<
>  >    -
⚪
⚫  G    G
G   .  G
"""

  val tbl_g = 
"""
 *.-<>⚪⚫G
*       F
.
-
<
>
⚪
⚫
GF      F
"""

  val productionRules = Map(
    dot-> processTable( tbl_dot),
    dash -> processTable( tbl_dash),
    lt -> processTable( tbl_lt),
    gt -> processTable( tbl_gt),
    circle -> processTable( tbl_circle),
    splot -> processTable( tbl_splot),
    g -> processTable( tbl_g)
  )

}

object SixState extends FS {
  val x :: l :: a :: b :: c :: g :: f :: dont_care :: Nil = (0 until 8).toList

  def boundary = x
  def general = g
  def init = l
  def fire = f


  val m = Map(
    'X' -> boundary,
    'L' -> l,
    'A' -> a,
    'B' -> b,
    'C' -> c,
    'G' -> g,
    'F' -> f,
    ' ' -> dont_care
  )

/*
  val tbl_l =
"""
 XLABCG
X
L
ACG   C
B
CGA   G
GAC   A
"""

  val tbl_a =
"""
 XLABCG
X  F G
L   LG 
AF  BCB
BCG GCC
C
GC   CC
"""

  val tbl_b = 
"""
 XLABCG
X
L  G L
A G  L
B GA C
CLLA  L
GGCC  G
"""

  val tbl_c = 
"""
 XLABCG
X
L  AG G
ABB B B
BG    G
C  AB B
GBB B B
"""

  val tbl_g = 
"""
 XLABCG
X A   F
L
A B
B B
CAA   A
GFB   F
"""
 */

  val tbl_l =
"""
 XLABCG
X L
LLL LLL
ACGLLLC
BLLLLLL
CGALLLG
GACLLLA
"""

  val tbl_a =
"""
 XLABCG
X  F G
L  ALG 
AFAABCB
BCG GCC
C AA
GC   CC
"""

  val tbl_b = 
"""
 XLABCG
X
L  GBLB
A GBBL
B GABCB
CLLA  L
GGCC BG
"""

  val tbl_c = 
"""
 XLABCG
X
L CAGCG
ABB B B
BGC  CG
C CABCB
GBB B B
"""

  val tbl_g = 
"""
 XLABCG
X A GGF
L  GGG
A B GG
BGB GGG
CAA GGA
GFB GGF
"""

  val productionRules = Map(
    l -> processTable( tbl_l),
    a -> processTable( tbl_a),
    b -> processTable( tbl_b),
    c -> processTable( tbl_c),
    g -> processTable( tbl_g)
  )

}

abstract class FiringSquadSiteIfc extends Module {

  val io = IO(new Bundle {
    val start = Input(Bool())
    val ls = Input(UInt(4.W))
    val ps = Output(UInt(4.W))
    val rs = Input(UInt(4.W))
  })

  def fs : FS

}

class FiringSquadSite( val fs : FS = EightState119) extends FiringSquadSiteIfc {

  val state = RegInit( fs.init.U(4.W))

  val ns = WireInit( state)

  ns := fs.dont_care.U

  for ( (k,v) <- fs.productionRules) {
    when ( k.U === io.ps) {
      for ( (kk,vv) <- v) {
        when ( kk._1.U === io.ls && kk._2.U === io.rs) {
          ns := vv.U
        }
      }
    }
  }

  when ( io.ls === fs.boundary.U && io.start && io.ps === fs.init.U) {
    ns := fs.general.U
  }

  when ( ns === fs.dont_care.U && io.ps =/= fs.fire.U) {
    printf( "Dont care found: %d %d %d\n", io.ls, io.ps, io.rs)
  }

  state := ns
  io.ps := state

}

class FiringSquadArray( factory : () => FiringSquadSiteIfc, val n : Int) extends Module {

  val io = IO(new Bundle {
    val start = Input( Bool())
    val states = Output( Vec( n, UInt(4.W)))
  })

  val a = IndexedSeq.fill( n)( Module( factory()))

  (io.states zip a).foreach { case (s,u) =>
    u.io.start := io.start
    s := u.io.ps
  }

  val boundary = a(0).fs.boundary

  a(0).io.ls := boundary.U
  a(0).io.rs := a(1).io.ps

  a(n-1).io.ls := a(n-2).io.ps
  a(n-1).io.rs := boundary.U

  ((a.slice(0,n-2) zip a.slice(1,n-1)) zip a.slice(2,n)).foreach { case ((l,p),r) =>
    p.io.ls := l.io.ps
    p.io.rs := r.io.ps
  }



}
