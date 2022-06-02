package macros

import scala.reflect.ClassTag

object ScalaToCTest extends App {
  val ast = ScalaToC.scalaToC("addThree") { (x: Int) =>
    val arr = Array(1, 2)
    arr(0) = arr.length
    val y = arr(0)
  }
}
