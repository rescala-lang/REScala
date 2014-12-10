package tests.rescala


import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.turns.Engines.default
import rescala.{Signals, Var}


class DynamicSignalTestSuite extends AssertionsForJUnit with MockitoSugar {

  @Test def signalReEvaluatesTheExpressionWhenSomethingItDependsOnIsUpdated(): Unit = {
    val v = Var(0)
    var i = 1
    val s = Signals.dynamic(v) { s => v(s) + i }
    i = 2
    v.set(2)
    assert(s.now == 4)
  }

  @Test def theExpressionIsNoteEvaluatedEveryTimeGetValIsCalled(): Unit = {
    var a = 10
    val s = Signals.dynamic()(s => 1 + 1 + a)
    assert(s.now === 12)
    a = 11
    assert(s.now === 12)
  }


  @Test def simpleSignalReturnsCorrectExpressions(): Unit = {
    val s = Signals.dynamic()(s => 1 + 1 + 1)
    assert(s.now === 3)
  }

  @Test def theExpressionIsEvaluatedOnlyOnce(): Unit = {

    var a = 0
    val v = Var(10)
    val s1 = Signals.dynamic(v) { s => a += 1; v(s) % 10 }
    var s2 = Signals.dynamic(s1) { s => a }


    assert(a == 1)
    v.set(11)
    assert(a == 2)
    v.set(21)
    assert(a == 3)
  }

  @Test def handlersAreExecuted(): Unit = {

    var test = 0
    val v = Var(1)

    val s1 = Signals.dynamic(v) { s => 2 * v(s) }
    val s2 = Signals.dynamic(v) { s => 3 * v(s) }
    val s3 = Signals.dynamic(s1, s2) { s => s1(s) + s2(s) }

    s1.changed += { (_) => test += 1 }
    s2.changed += { (_) => test += 1 }
    s3.changed += { (_) => test += 1 }

    assert(test == 0)

    v.set(3)
    assert(test == 3)

  }

  @Test def levelIsCorrectlyComputed(): Unit = {

    val v = Var(1)

    val s1 = Signals.dynamic() { s => 2 * v(s) }
    val s2 = Signals.dynamic() { s => 3 * v(s) }
    val s3 = Signals.dynamic() { s => s1(s) + s2(s) }

    assert(v.getLevel == 0)
    assert(s1.getLevel == 1)
    assert(s2.getLevel == 1)
    assert(s3.getLevel == 2)


  }


  /* Specific of SignalSynt */


  @Test def signalDoesNotReEvaluateTheExpressionIfDependsOnIsUpdatedThatIsNotInCurrentDependencies(): Unit = {
    val v1 = Var(true)
    val v2 = Var(0)
    val v3 = Var(10)
    var i = 0
    val s = Signals.dynamic(v1, v2, v3) { s =>
      i += 1
      if (v1(s)) v2(s) else v3(s)
    }

    assert(i == 1)
    assert(s.now == 0)
    v2.set(1)
    assert(i == 2)
    assert(s.now == 1)
    v3.set(11) // No effect
    assert(i == 2)
    assert(s.now == 1)

    v1.set(false)
    assert(i == 3)
    assert(s.now == 11)
    v3.set(12)
    assert(i == 4)
    assert(s.now == 12)
    v2.set(2) // No effect
    assert(i == 4)
    assert(s.now == 12)
  }


  @Test def keep_fixedDependencies(): Unit = {

    val v1 = Var(true)
    val v2 = Var(0)
    val v3 = Var(10)
    var i = 0
    var test = 0

    val s = Signals.dynamic() { s =>
      i += 1
      if (v1(s)) v2(s) else v3(s)
    }

    val e = s.change
    e += ((x: (Int, Int)) => test += 1)

    assert(test == 0)
    v2.set(1)
    assert(test == 1)

    v1.set(false)
    assert(test == 2)
    v3.set(11)
    assert(test == 3)

    v2.set(2)
    assert(test == 3)

    v1.set(true)
    assert(test == 4)
    v2.set(3)
    assert(test == 5)


  }

  @Test def dependantIsOnlyInvokedOnValueChanges(): Unit = {
    var changes = 0
    val v = Var(1)
    val s = Signals.dynamic(v) { s =>
      changes += 1; v(s) + 1
    }
    assert(changes == 1)
    assert(s.now == 2)
    v.set(2)
    assert(changes == 2)
    v.set(2)
    assert(changes == 2) // is actually 3
  }

  @Test def creatingSignalsInsideSignals(): Unit = {

    val outside = Var(1)

    val testsig = Signals.dynamic() { t =>
      //remark 01.10.2014: without the upper bound the inner signal will be enqueued (it is level 0 same as its dependency)
      //this will cause testsig to reevaluate again, after the inner signal is fully updated.
      // leading to an infinite loop
      Signals.dynamic(outside) { t => outside(t) }.apply(t)
    }

    assert(testsig.now === 1)
    outside() = 2
    assert(testsig.now === 2)
  }

}
