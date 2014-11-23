package rescala.test

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.signals._
import rescala.propagation.turns.creation.TurnFactory

class SignalTestSuite extends AssertionsForJUnit with MockitoSugar {

  @Test def signalReEvaluatesTheExpression(): Unit = {
    val v  = Var(0)
    var i = 1
    val s: Signal[Int] = v.map { _ => i }
    i = 2
    v.set(2)
    assert(s.get == 2)
  }

  @Test def theExpressionIsNoteEvaluatedEveryTimeGetValIsCalled(): Unit = {
    var a = 10
    val s: Signal[Int] = Signals.mapping()(_ => 1 + 1 + a )
    assert(s.get === 12)
    a = 11
    assert(s.get === 12)
  }


  @Test def simpleSignalReturnsCorrectExpressions(): Unit = {
    val s: Signal[Int] = Signals.mapping()(_ => 1 + 1 + 1 )
    assert(s.get === 3)
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

  @Test def handlersAreExecuted() =  {

    var test = 0
    val v = Var(1)

    val s1 = v.map { 2 * _ }
    val s2 = v.map { 3 * _ }
    val s3 = Signals.lift(s1, s2){ _ + _ }

    s1.changed += { (_) => test += 1 }
    s2.changed += { (_) => test += 1 }
    s3.changed += { (_) => test += 1 }

    assert(test == 0)

    v.set(3)
    assert(test == 3)

  }

  @Test def levelIsCorrectlyComputed() =  {

    val v = Var(1)

    val s1 = v.map { 2 * _ }
    val s2 = v.map { 3 * _ }
    val s3 = Signals.lift(s1,s2){ _ + _ }

    implicitly[TurnFactory].newTurn { implicit turn =>
      assert(v.getLevel == 0)
      assert(s1.getLevel == 1)
      assert(s2.getLevel == 1)
      assert(s3.getLevel == 2)
    }
  }

}
