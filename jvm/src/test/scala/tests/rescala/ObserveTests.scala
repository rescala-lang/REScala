package tests.rescala

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.reactives.Observe.once
import rescala.engines.Engine
import rescala.graph.Struct
import rescala.propagation.Turn
import rescala.reactives.Events
import rescala.reactives.Signals

object ObserveTests extends JUnitParameters

@RunWith(value = classOf[Parameterized])
class ObserveTests[S <: Struct](engine: Engine[S, Turn[S]]) extends AssertionsForJUnit with MockitoSugar {
  implicit val implicitEngine: Engine[S, Turn[S]] = engine
  import implicitEngine.{Event, Signal, Var}

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
        t.drop(link)(v1)
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
      if (v > 10) t.drop(obs)(v1)
      else t.schedule(once[Int](obs, Some(v), result ::= _))
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
