package scala.events.test
import scala.events.scalareact
import scala.events.behaviour.Var

object Example2 extends App {
  val v = scala.events.behaviour.Var(0)
  val dep = scala.events.behaviour.Signal { v() + 10 }
  println(dep.getValue)

  println("---")
  v() = 1

  println(dep.getValue)
}