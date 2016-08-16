package tests.rescala

import rescala.Infiltrator.assertLevel
import rescala.reactives.Signals


class SignalTestSuite extends RETests {





  allEngines("signal ReEvaluates The Expression"){ engine => import engine._
    val v = Var(0)
    var i = 1
    val s: Signal[Int] = v.map { _ => i }
    i = 2
    v.set(2)
    assert(s.now == 2)
  }

  allEngines("the Expression IsNote Evaluated Every Time Get Val IsCalled"){ engine => import engine._
    var a = 10
    val s: Signal[Int] = Signals.static()(_ => 1 + 1 + a)
    assert(s.now === 12)
    a = 11
    assert(s.now === 12)
  }


  allEngines("simple Signal Returns Correct Expressions"){ engine => import engine._
    val s: Signal[Int] = Signals.static()(_ => 1 + 1 + 1)
    assert(s.now === 3)
  }

  allEngines("the Expression IsEvaluated Only Once"){ engine => import engine._

    var a = 0
    val v = Var(10)
    val s1: Signal[Int] = v.map { i =>
      a += 1
      i % 10
    }


    assert(a == 1)
    v.set(11)
    assert(a == 2)
    v.set(21)
    assert(a == 3)
  }

  allEngines("handlers Are Executed"){ engine => import engine._

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

  allEngines("level IsCorrectly Computed"){ engine => import engine._

    val v = Var(1)

    val s1 = v.map { 2 * _ }
    val s2 = v.map { 3 * _ }
    val s3 = Signals.lift(s1, s2) { _ + _ }

    assertLevel(v, 0)
    assertLevel(s1, 1)
    assertLevel(s2, 1)
    assertLevel(s3, 2)
  }


  allEngines("no Change Propagations"){ engine => import engine._
    val v = Var(1)
    val s = v.map(_ => 1)
    val s2 = Signal{ s() }

    assert(s2.now === 1)
    assert(s.now === 1)

    v.set(2)
    assert(s2.now === 1)
    assert(s.now === 1)


    v.set(2)
    assert(s2.now === 1)
    assert(s.now === 1)


    v.set(3)
    assert(s2.now === 1)
    assert(s.now === 1)


  }

}
