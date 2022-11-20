import spray.json._
import scala.io.Source
import java.io._

object MergeJSON extends App {

  val JsArray( lst0) = Source.fromFile("tradeoffRatio.json")("UTF-8").mkString.parseJson
  val JsArray( lst1) = Source.fromFile("tradeoffPeriod.json")("UTF-8").mkString.parseJson

  val jsonAST = JsArray( lst0 ++ lst1)

  val fp = new File( "tradeoff.json")
  val bw = new BufferedWriter(new FileWriter( fp))
  bw.write( jsonAST.prettyPrint)
  bw.close()

}
