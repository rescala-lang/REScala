package rescala.macros
import rescala.default.*

object Main {
  def main(args: Array[String]): Unit = {
    val iAmAnEvt = Evt[Int]()
    val iAmToo   = Evt[Int]()
    val iAmThree = Evt[Int]()

    def iReturnThee = iAmThree

    val derived = dottyEventExpression(iAmAnEvt.value)

    derived.observe(println)

    //dottyEventExpression {
    //  val dep = iAmToo.value
    //  dep.map(_.toString)
    //}

    //Events.dynamic() { dt =>
    //  {
    //    implicit def idt: DynamicTicket = dt
    //    dottyEventExpression({
    //      (
    //        iAmAnEvt.value,
    //        ((f: Int) => f.toString + iAmToo.value),
    //        iAmThree, // note, note value
    //        iReturnThee.value,
    //      )
    //    })
    //    println("reevaluated!")
    //    None
    //  }
    //}
    iAmAnEvt.fire(5)
    iAmThree.fire(6)
  }
}
