package rescala.macros
import rescala.default.*

object Main {
  def main(args: Array[String]): Unit = {
    val iAmAnEvt = Evt[Int]()
    val iAmToo   = Evt[Int]()
    val iAmThree = Evt[Int]()

    def iReturnThee = iAmThree

    val derived = dottyEventExpression[Int] {
      def a: Signal[Int] = Var(5)
      val b = 4
      class c()
      object d
      (iAmAnEvt.value, Some(a.value): Option[Int]) match {
        case (None, b)          => b
        case (a, None)          => a
        case (Some(a), Some(b)) => Some(a + b)
        case other              => None
      }
    }

    derived.observe(println)

    // Events.dynamic() { dt =>
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
    // }
    iAmToo.fire(10)
    iAmAnEvt.fire(5)
    iAmThree.fire(6)
  }
}
