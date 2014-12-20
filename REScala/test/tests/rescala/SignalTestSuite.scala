package tests.rescala

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.Infiltrator.getLevel
import rescala.turns.{Engine, Turn}
import rescala.{Signal, Signals, Var}

object SignalTestSuite extends JUnitParameters

@RunWith(value = classOf[Parameterized])
class SignalTestSuite(engine: Engine[Turn]) extends AssertionsForJUnit with MockitoSugar {
  implicit val implicitEngine: Engine[Turn] = engine


  @Test def signalReEvaluatesTheExpression(): Unit = {
    val v = Var(0)
    var i = 1
    val s: Signal[Int] = v.map { _ => i }
    i = 2
    v.set(2)
    assert(s.now == 2)
  }

  @Test def theExpressionIsNoteEvaluatedEveryTimeGetValIsCalled(): Unit = {
    var a = 10
    val s: Signal[Int] = Signals.mapping()(_ => 1 + 1 + a)
    assert(s.now === 12)
    a = 11
    assert(s.now === 12)
  }


  @Test def simpleSignalReturnsCorrectExpressions(): Unit = {
    val s: Signal[Int] = Signals.mapping()(_ => 1 + 1 + 1)
    assert(s.now === 3)
  }

  @Test def theExpressionIsEvaluatedOnlyOnce(): Unit = {

    var a = 0
    val v = Var(10)
    val s1: Signal[Int] = v.map { i => a += 1; i % 10 }


    assert(a == 1)
    v.set(11)
    assert(a == 2)
    v.set(21)
    assert(a == 3)
  }

  @Test def handlersAreExecuted() = {

    var test = 0
    val v = Var(1)

    val s1 = v.map { 2 * _ }
    val s2 = v.map { 3 * _ }
    val s3 = Signals.lift(s1, s2) { _ + _ }

    s1.changed += { (_) => test += 1 }
    s2.changed += { (_) => test += 1 }
    s3.changed += { (_) => test += 1 }

    assert(test == 0)

    v.set(3)
    assert(test == 3)

  }

  @Test def levelIsCorrectlyComputed() = {

    val v = Var(1)

    val s1 = v.map { 2 * _ }
    val s2 = v.map { 3 * _ }
    val s3 = Signals.lift(s1, s2) { _ + _ }

    assert(getLevel(v) == 0)
    assert(getLevel(s1) == 1)
    assert(getLevel(s2) == 1)
    assert(getLevel(s3) == 2)
  }

}
