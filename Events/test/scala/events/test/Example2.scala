package scala.events.test
import scala.events.scalareact
import scala.events.behaviour.Var
import scala.events.ImperativeEvent
import scala.events.behaviour.Signal

object Example2 extends App {
  /**
   * val v = Var(0)
   * val dep = Signal { v() + 10 }
   * println(dep.getValue)
   *
   * println("---")
   * v() = 1
   *
   * println(dep.getValue)
   */

  val e = new ImperativeEvent[Double]

  val all = e.list() // represents all assigned values
  val window = e.last(5) // takes the last five assigned values
  val mean = Signal { window().sum / (window().length + 1) } // 'mean' changes after 'e' is reassigned

  mean.changed += { println(_) } // prints new value of 'mean' after new value

  e(1); e(2); e(3); e(4); e(5); e(6)
}