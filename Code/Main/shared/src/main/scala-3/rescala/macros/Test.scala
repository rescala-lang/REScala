package rescala.macros
import rescala.default._

object Main {
  def main(args: Array[String]): Unit = {
    val iAmAnEvt = Evt[Int]()
    val iAmToo = Evt[Int]()
    val iAmThree = Evt[Int]()
    detectReactives( {
      (iAmAnEvt.value,
          ((f: Int) => f.toString + iAmToo.value),
          iAmThree // note, note value)
    )})
  }
}
