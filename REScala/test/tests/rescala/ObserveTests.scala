package tests.rescala

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.Observe.once
import rescala.turns.{Engine, Turn}
import rescala.{Event, Events, Signal, Signals, Var}

object ObserveTests extends JUnitParameters

@RunWith(value = classOf[Parameterized])
class ObserveTests(engine: Engine[Turn]) extends AssertionsForJUnit with MockitoSugar {
  implicit val implicitEngine: Engine[Turn] = engine

  @Test def `can observe signals`(): Unit = {
    var result = List[Int]()
    val v1 = Var(0)
    v1.observe(result ::= _)

    assert(result === List(0))

    v1.set(10)

    assert(result === List(10, 0))
  }

  @Test def `self removing observers are possible, although maybe not as straight forward as one would wish?`(): Unit = {
    var result = List[Int]()
    val v1 = Var(0)
    lazy val link: Signal[Int] = Signals.static(v1) { t =>
      val v = v1.get(t)
      if (v > 10) {
        t.unregister(link)(v1)
        obs.remove()
      }
      v
    }

    lazy val obs = link.observe(result ::= _)

    // we need this to force the observer into existence
    obs

    assert(result === List(0))
    v1.set(10)
    assert(result === List(10, 0))
    v1.set(20)
    assert(result === List(10, 0))
    v1.set(5)
    assert(result === List(10, 0))
  }

  @Test def `simpler self removing observers, but this does not fire on attach`(): Unit = {
    var result = List[Int]()
    val v1 = Var(0)

    lazy val obs: Event[Int] = Events.static("obs", v1) { t =>
      val v = v1.get(t)
      if (v > 10) t.unregister(obs)(v1)
      else t.plan(once[Int](obs, Some(v), result ::= _))
      v1.pulse(t)
    }

    // we need this to force the observer into existence
    obs

    assert(result === List())
    v1.set(10)
    assert(result === List(10))
    v1.set(20)
    assert(result === List(10))
    v1.set(5)
    assert(result === List(10))
  }

}
