package tests.rescala

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.Infiltrator.getLevel
import rescala.graph.Spores
import rescala.turns.{Engine, Turn}

object MacroTestSuite extends JUnitParameters

@RunWith(value = classOf[Parameterized])
class MacroTestSuite[S <: Spores](engine: Engine[S, Turn[S]]) extends AssertionsForJUnit with MockitoSugar {
  implicit val implicitEngine: Engine[S, Turn[S]] = engine
  import implicitEngine.{Evt, Var, Signal, Event}

  @Test def signalReEvaluatesTheExpression(): Unit = {
    val v = Var(0)
    var i = 1
    val s: Signal[Int] = Signal { v(): @unchecked; i }
    i = 2
    v.set(2)
    assert(s.now === 2)
  }

  @Test def theExpressionIsNotEvaluatedEveryTimeGetValIsCalled(): Unit = {
    var a = 10
    val s: Signal[Int] = Signal { 1 + 1 + a }
    assert(s.now === 12)
    a = 11
    assert(s.now === 12)
  }


  @Test def simpleSignalReturnsCorrectExpressions(): Unit = {
    val s: Signal[Int] = Signal(1 + 1 + 1)
    assert(s.now === 3)
  }

  @Test def theExpressionIsEvaluatedOnlyOnce(): Unit = {

    var a = 0
    val v = Var(10)
    val s1: Signal[Int] = Signal { a += 1; v() % 10 }


    assert(a === 1)
    v.set(11)
    assert(a === 2)
    v.set(21)
    assert(a === 3)
  }

  @Test def handlersAreExecuted(): Unit = {

    var test = 0
    val v = Var(1)

    val s1 = Signal { 2 * v() }
    val s2 = Signal { 3 * v() }
    val s3 = Signal { s1() + s2() }

    s1.changed += { (_) => test += 1 }
    s2.changed += { (_) => test += 1 }
    s3.changed += { (_) => test += 1 }

    assert(test === 0)

    v.set(3)
    assert(test === 3)

  }

  @Test def levelIsCorrectlyComputed(): Unit = {

    val v = Var(1)

    val s1 = Signal { 2 * v() }
    val s2 = Signal { 3 * v() }
    val s3 = Signal { s1() + s2() }

    assert(getLevel(v) === 0)
    assert(getLevel(s1) === 1)
    assert(getLevel(s2) === 1)
    assert(getLevel(s3) === 2)


  }


  @Test def conversionFunctionWithArgumentInSignal(): Unit = {

    var test = 0
    val e = Evt[Int]()
    val s: Signal[Int] = Signal { 2 * e.latest(0)(implicitly)() }

    s.change += { _ => test += 1 }
    assert(s.now === 0)
    e(2)
    assert(s.now === 4)
    e(3)
    assert(s.now === 6)
    assert(test === 2)
  }


  @Test def conversionFunctionWithoutArgumentInSignal(): Unit = {

    var test = 0
    val e = Evt[Int]()
    val s: Signal[Option[Int]] = Signal { e.latestOption()(implicitly)() }

    s.change += { _ => test += 1 }
    assert(s.now === None)
    e(2)
    assert(s.now === Some(2))
    e(3)
    assert(s.now === Some(3))
    assert(test === 2)
  }


  @Test def conversionFunctionsWorkInSignalsInObjectConstructionInOverridenDef(): Unit = {
    // a previous macro implementation yielded wrong results for code of the
    // following form, see:
    // https://github.com/guidosalva/examples/pull/4/files#r11724000
    var test = 0
    var e = null: rescala.Evt[Int, S]
    var s = null: Signal[Int]

    abstract class A {def obj(): Unit }
    val a = new A {
      def obj() = new {
        val evt = Evt[Int]()
        val sig: Signal[Int] = Signal { 2 * evt.latest(0)(implicitly)() }

        e = evt
        s = sig
      }
    }

    a.obj()
    s.change += { _ => test += 1 }
    assert(s.now === 0)
    e(2)
    assert(s.now === 4)
    e(3)
    assert(s.now === 6)
    assert(test === 2)
  }


  @Test def signalsNestedInVars(): Unit = {

    val a = Var(3)
    val b = Var(Signal(a()))
    val c = Signal(b()())

    assert(c.now === 3)
    a() = 4
    assert(c.now === 4)
    b() = Signal(5)
    assert(c.now === 5)

  }


  @Test def nestedDefinedSignals(): Unit = {
    val a = Var(3)
    val b = Signal {
      val c = Signal { a() }
      c()
    }

    assert(b.now === 3)
    a() = 4
    assert(b.now === 4)
    a() = 5
    assert(b.now === 5)
  }


  @Test def useOfInsideSignal(): Unit = {
    val outside = Var(1)
    val inside = Var(10)

    def sig = Signal { outside() }

    val testsig = Signal {
      def sig = Signal { inside() }
      sig()
    }

    assert(testsig.now === 10)
    outside() = 2
    inside() = 11
    assert(testsig.now === 11)
  }

  @Test def useOfOutsideSignal(): Unit = {
    val outside = Var(1)
    val inside = Var(10)

    def sig = Signal { outside() }

    val testsig = Signal {
      {
        def sig = Signal { inside() }
        sig()
      }
      sig()
    }

    assert(testsig.now === 1)
    outside() = 2
    inside() = 11
    assert(testsig.now === 2)
  }


  // Assumingly, this test fails due to https://issues.scala-lang.org/browse/SI-5465
  // We should check if the test succeeds when the issue is fixed
  //
  //  @Test def patternMatchingAndWildcard() = {
  //    val v1 = Var(List(1, 2))
  //    val v2 = Var(100)
  //
  //    val sig = Signal {
  //      v1() match {
  //	      case List(_, v) => v
  //	      case _ => v2()
  //	    }
  //    }
  //
  //    assert(sig.get===2)
  //    v2() = 50
  //    assert(sig.get===2)
  //    v1() = List(7, 8, 9)
  //    assert(sig.get===50)
  //    v2() = 4
  //    assert(sig.get===4)
  //    v1() = List(10, 11)
  //    assert(sig.get===11)
  //  }


  @Test def patternMatchingAnonymousFunction(): Unit = {
    val s1 = Signal { List(Some(1), Some(2), None, Some(4), None) }
    val s2 = Signal {
      s1() collect { case Some(n) => n }
    }
    assert(s2.now === List(1, 2, 4))
  }

  @Test def patternMatchingAnonymousFunctionNestedSignals(): Unit = {
    val v1 = Var(1)
    val v2 = Var(2)
    val s1 = Signal { List(Some(v1), None, Some(v2), None) }
    val s2 = Signal {
      s1() collect { case Some(n) => n() }
    }
    assert(s2.now === List(1, 2))
    v1.set(10)
    assert(s2.now === List(10, 2))
  }

  @Test def outerAndInnerValues(): Unit = {
    val v = Var(0)
    object obj {
      def sig = Signal { v() }
    }

    val evt = Evt[Int]()

    val testsig = Signal {
      val localsig = obj.sig
      val latest = evt latest -1

      localsig() + latest()
    }

    assert(testsig.now === -1)
    evt(100)
    assert(testsig.now === 100)
    v() = 10
    assert(testsig.now === 110)
    evt(10)
    assert(testsig.now === 20)
    evt(5)
    assert(testsig.now === 15)
    v() = 50
    assert(testsig.now === 55)
  }


  @Test def chainedSignals1(): Unit = {
    import scala.language.reflectiveCalls

    val v1 = Var { 1 }
    val v2 = Var { 2 }
    val v = Var { List(new {val s = v1 }, new {val s = v2 }) }

    val sig = Signal { v() map (_.s()) }

    assert(sig.now === List(1, 2))
    v1() = 5
    assert(sig.now === List(5, 2))
    v2() = 7
    assert(sig.now === List(5, 7))
    v() = v.now.reverse
    assert(sig.now === List(7, 5))
  }

  @Test def chainedSignals2(): Unit = {
    import scala.language.reflectiveCalls

    val v1 = Var { 20 }
    val v2 = Var { new {def signal = Signal { v1() } } }
    val v3 = Var { new {val signal = Signal { v2() } } }

    val s = Signal { v3() }

    val sig = Signal { s().signal().signal(): @unchecked }

    assert(sig.now === 20)
    v1() = 30
    assert(sig.now === 30)
    v2() = new {def signal = Signal { 7 + v1() } }
    assert(sig.now === 37)
    v1() = 10
    assert(sig.now === 17)
    v3() = new {val signal = Signal { new {def signal = Signal { v1() } } } }
    assert(sig.now === 10)
    v2() = new {def signal = Signal { 10 + v1() } }
    assert(sig.now === 10)
    v1() = 80
    assert(sig.now === 80)
  }


  @Test def functionAsGetterForSignal(): Unit = {
    import scala.language.reflectiveCalls

    def getSignal(obj: {def signal: Signal[Int]}) = obj.signal

    val v = Var { 20 }
    val o = new {val signal = Signal { v() } }

    val sig = Signal { getSignal(o)() }

    assert(sig.now === 20)
    v() = 30
    assert(sig.now === 30)
  }


  @Test def functionAsGetterForEventAndConversionFunction(): Unit = {
    import scala.language.reflectiveCalls

    def getSignal(obj: {def evt: Event[Int]}) = obj.evt

    val e = Evt[Int]()
    val o = new {val evt = e }

    val sig = Signal { getSignal(o).latestOption()(implicitly)() }

    assert(sig.now === None)
    e(30)
    assert(sig.now === Some(30))
  }
}
