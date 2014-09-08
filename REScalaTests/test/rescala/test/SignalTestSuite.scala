package rescala.test

import org.junit.{Before, Test}
import org.mockito.Mockito.verify
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala._
import rescala.propagation.Turn
import rescala.signals._

class SignalTestSuite extends AssertionsForJUnit with MockitoSugar {

  @Before def initialize(): Unit = {}

  @Test def dependencyHolderNotifiesDependentsWhenNotifyDependentsIsCalled(): Unit = {

    val dh = new {} with Dependency[Unit] {}
    val v  = Var(0)
    val s1 = mock[DependentSignal[Int]]
    val s2 = mock[DependentSignal[Int]]
    val s3 = mock[DependentSignal[Int]]

    dh.addDependant(s1)
    dh.addDependant(s2)
    dh.addDependant(s3)
    Turn.newTurn { turn =>
      dh.notifyDependants(turn)
      verify(s1).dependencyChanged(dh)(turn)
      verify(s2).dependencyChanged(dh)(turn)
      verify(s3).dependencyChanged(dh)(turn)
    }

  }

  @Test def signalReEvaluatesTheExpression(): Unit = {
    val v  = Var(0)
    var i = 1
    val s: Signal[Int] = StaticSignal(v) { i }
    i = 2
    v.set(2)
    assert(s.get == 2)
  }

  @Test def theExpressionIsNoteEvaluatedEveryTimeGetValIsCalled(): Unit = {
    var a = 10
    val s: Signal[Int] = StaticSignal(List())( 1 + 1 + a )
    assert(s.get === 12)
    a = 11
    assert(s.get === 12)
  }


  @Test def simpleSignalReturnsCorrectExpressions(): Unit = {
    var s: Signal[Int] = StaticSignal(List())( 1 + 1 + 1 )
    assert(s.get === 3)
  }

  @Test def theExpressionIsEvaluatedOnlyOnce(): Unit = {

    var a = 0
    val v = Var(10)
    val s1: Signal[Int] = StaticSignal(v) { a += 1; v.get % 10 }


    assert(a == 1)
    v.set(11)
    assert(a == 2)
    v.set(21)
    assert(a == 3)
  }

  @Test def handlersAreExecuted() =  {

    var test = 0
    val v = Var(1)

    val s1 = StaticSignal(v){ 2 * v.get }
    val s2 = StaticSignal(v){ 3 * v.get }
    val s3 = StaticSignal(s1,s2){ s1.get + s2.get }

    s1.changed += { (_) => test += 1 }
    s2.changed += { (_) => test += 1 }
    s3.changed += { (_) => test += 1 }

    assert(test == 0)

    v.set(3)
    assert(test == 3)

  }

  @Test def levelIsCorrectlyComputed() =  {

    var test = 0
    val v = Var(1)

    val s1 = StaticSignal(v){ 2 * v.get }
    val s2 = StaticSignal(v){ 3 * v.get }
    val s3 = StaticSignal(s1,s2){ s1.get + s2.get }

    assert(v.level == 0)
    assert(s1.level == 1)
    assert(s2.level == 1)
    assert(s3.level == 2)
  }

}
