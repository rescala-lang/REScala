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

  val a = new ImperativeEvent[Int]
  val b = new ImperativeEvent[Int]

  val either = a || b

  either += (x => println("either: " + x))
  a += (x => println("a: " + x))

  a(1)
  b(2)
}