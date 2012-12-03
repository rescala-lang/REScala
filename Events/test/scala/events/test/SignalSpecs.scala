package scala.events.test

import collection.mutable.Stack
import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.events.behaviour.Var
import scala.events.behaviour.Signal

@RunWith(classOf[JUnitRunner])
class SignalSpecs extends FlatSpec with ShouldMatchers {

  def a_plus_b =
    new {
      val a = Var(0)
      val b = Var(0)
      val signal = Signal { a() + b() }
    }

  "A Signal" should "depend on Vars" in {
    val a = Var(1)
    val b = Var(2)
    val signal = Signal { a() + b() }
  }

  it should "return its current value with getValue" in {
    val f = a_plus_b

    f.a() = 3
    f.b() = 4
    f.signal.getValue should equal(7)

    f.a() = 0
    f.signal.getValue should equal(4)
  }

  it should "emit changed events" in {

  }
}