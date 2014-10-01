package rescala.test


import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.makro.SignalMacro
import rescala.propagation.Turn
import rescala.signals._


class SignalSyntTestSuite extends AssertionsForJUnit with MockitoSugar {

  @Test def signalReEvaluatesTheExpressionWhenSomethingItDependsOnIsUpdated(): Unit = {
    val v = Var(0)
    var i = 1
    val s: DynamicSignal[Int] = DynamicSignal[Int](v) { s => v(s) + i }
    i = 2
    v.set(2)
    assert(s.get == 4)
  }

  @Test def theExpressionIsNoteEvaluatedEveryTimeGetValIsCalled(): Unit = {
    var a = 10
    val s: DynamicSignal[Int] = DynamicSignal[Int](List())(s => 1 + 1 + a)
    assert(s.get === 12)
    a = 11
    assert(s.get === 12)
  }


  @Test def simpleSignalReturnsCorrectExpressions(): Unit = {
    val s: DynamicSignal[Int] = DynamicSignal[Int](List())(s => 1 + 1 + 1)
    assert(s.get === 3)
  }

  @Test def theExpressionIsEvaluatedOnlyOnce(): Unit = {

    var a = 0
    val v = Var(10)
    val s1 = DynamicSignal(v) { s => a += 1; v(s) % 10 }
    var s2 = DynamicSignal(s1) { s => a }


    assert(a == 1)
    v.set(11)
    assert(a == 2)
    v.set(21)
    assert(a == 3)
  }

  @Test def handlersAreExecuted() = {

    var test = 0
    val v = Var(1)

    val s1 = DynamicSignal[Int]() { s => 2 * v(s) }
    val s2 = DynamicSignal[Int]() { s => 3 * v(s) }
    val s3 = DynamicSignal[Int]() { s => s1(s) + s2(s) }

    s1.changed += { (_) => test += 1 }
    s2.changed += { (_) => test += 1 }
    s3.changed += { (_) => test += 1 }

    assert(test == 0)

    v.set(3)
    assert(test == 3)

  }

  @Test def levelIsCorrectlyComputed() = {

    val v = Var(1)

    val s1 = DynamicSignal[Int]() { s => 2 * v(s) }
    val s2 = DynamicSignal[Int]() { s => 3 * v(s) }
    val s3 = DynamicSignal[Int]() { s => s1(s) + s2(s) }

    Turn.newTurn { implicit turn =>
      assert(v.level == 0)
      assert(s1.level == 1)
      assert(s2.level == 1)
      assert(s3.level == 2)
    }


  }


  /* Specific of SignalSynt */


  @Test def signalDoesNotReEvaluateTheExpressionIfDependsOnIsUpdatedThatIsNotInCurrentDependencies(): Unit = {
    val v1 = Var(true)
    val v2 = Var(0)
    val v3 = Var(10)
    var i = 0
    val s: DynamicSignal[Int] = DynamicSignal[Int](v1, v2, v3) { s =>
      i += 1
      if (v1(s)) v2(s) else v3(s)
    }

    assert(i == 1)
    assert(s.get == 0)
    v2.set(1)
    assert(i == 2)
    assert(s.get == 1)
    v3.set(11) // No effect
    assert(i == 2)
    assert(s.get == 1)

    v1.set(false)
    assert(i == 3)
    assert(s.get == 11)
    v3.set(12)
    assert(i == 4)
    assert(s.get == 12)
    v2.set(2) // No effect
    assert(i == 4)
    assert(s.get == 12)
  }


  @Test def keep_fixedDependencies(): Unit = {

    val v1 = Var(true)
    val v2 = Var(0)
    val v3 = Var(10)
    var i = 0
    var test = 0

    val s = DynamicSignal { s =>
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
    val s = DynamicSignal(v) { s =>
      changes += 1; v(s) + 1
    }
    assert(changes == 1)
    assert(s.get == 2)
    v.set(2)
    assert(changes == 2)
    v.set(2)
    assert(changes == 2) // is actually 3
  }

}
