package rescala.test.log

import org.junit.Before
import org.junit.Test
import org.mockito.Mockito.times
import org.mockito.Mockito.verify
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar

import makro.SignalMacro.{ SignalM => Signal }
import rescala.Handler
import rescala.Signal
import rescala.Var
import rescala.events.ImperativeEvent
import rescala.log.NoLogging

class LoggingTest extends AssertionsForJUnit with MockitoSugar {

  var log: NoLogging = _

  @Before def initialize() {
    log = mock[NoLogging]
    rescala.ReactiveEngine.log = log
  }

  @Test def nodeCreatedIsLogged() {
    val v = Var(Unit)
    val s = Signal {}
    val e = new ImperativeEvent
    val eh = Handler {}

    verify(log).nodeCreated(v)
    verify(log).nodeCreated(s)
    verify(log).nodeCreated(e)
    verify(log).nodeCreated(eh)
  }

  @Test def nodeAttachedIsLogged() {
    val v1 = Var(1)
    val v2 = Var(1)
    val s = Signal { v1() + v2() }

    verify(log).nodeAttached(s, v1)
    verify(log).nodeAttached(s, v2)
  }

  @Test def nodePulsedIsLogged() {
    // TODO implement
  }

  @Test def nodeScheduledIsLogged() {
    // TODO implement
  }

  @Test def nodeEvaluationStartedIsLoggedOnInitialization() {
    val s = Signal {}

    verify(log).nodeEvaluationStarted(s)
  }

  @Test def nodeEvaluationStartedIsLoggedOnValueChange() {
    val v1 = Var(1)
    val v2 = Var(1)
    val s = Signal { v1() + v2() }
    v1.set(2)

    verify(log, times(2)).nodeEvaluationStarted(s)
  }

  @Test def nodeEvaluationStartedIsLoggedOnValueChangeMultipleTimes() {
    val v1 = Var(1)
    val v2 = Var(1)
    val s = Signal { v1() + v2() }
    v1.set(2)
    v1.set(3)
    v2.set(10)

    verify(log, times(4)).nodeEvaluationStarted(s)
  }

  @Test def nodeEvaluationEndedIsLoggedOnInitialization() {
    val s = Signal {}

    verify(log).nodeEvaluationEnded(s)
  }

  @Test def nodeEvaluationEndedIsLoggedOnValueChange() {
    val v1 = Var(1)
    val v2 = Var(1)
    val s = Signal { v1() + v2() }
    v1.set(2)

    verify(log, times(2)).nodeEvaluationEnded(s)
  }

  @Test def nodeEvaluationEndedIsLoggedOnValueChangeMultipleTimes() {
    val v1 = Var(1)
    val v2 = Var(1)
    val s = Signal { v1() + v2() }
    v1.set(2)
    v1.set(3)
    v2.set(10)

    verify(log, times(4)).nodeEvaluationEnded(s)
  }

  @Test def nodeEvaluationEndedWithExceptionIsLogged() {
    val v = Var(true)
    var s: Signal[String] = null
    try {
      s = Signal { if (v()) { "All good" } else { throw new Exception } }
      v.set(false)
    } catch {
      case e: Exception => verify(log).nodeEvaluationEndedWithException(s, e)
    }
  }

  @Test def nodeValueSetIsLogged() {
    val v = Var(1)
    v.set(2)

    verify(log).nodeValueSet(v)
  }

  @Test def nodeValueSetIsLoggedMultipleTimes() {
    val v = Var(1)
    v.set(3)
    v.set(4)

    verify(log, times(2)).nodeValueSet(v)
  }

  @Test def nodePropagationStoppedIsLogged() {
    // TODO implement
  }

  @Test def logRoundIsLogged() {
    // TODO implement
  }

  @Test def logMessageIsLogged() {
    // TODO implement
  }

}
