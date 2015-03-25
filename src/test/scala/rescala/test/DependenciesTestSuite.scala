package rescala.test

import org.junit.Before
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar

import rescala._
import makro.SignalMacro.{ SignalM => Signal }
import rescala.events._

class DependenciesTestSuite extends AssertionsForJUnit with MockitoSugar {

  @Before def initialize() {}

  @Test def varsDoNotHaveDependentsByDefault() {
    val v1 = Var(0)
    val v2 = Var("Hello World")
    val v3 = Var(false)
    assert(v1.dependentCount() == 0)
    assert(v2.dependentCount() == 0)
    assert(v3.dependentCount() == 0)
  }

  @Test def signalsDoNotHaveDependenciesByDefault() {
    val s1 = Signal { 0 }
    val s2 = Signal { "Hello World" }
    val s3 = Signal { false }
    assert(s1.dependentCount() == 0)
    assert(s1.dependOnCount() == 0)
    assert(s2.dependentCount() == 0)
    assert(s2.dependOnCount() == 0)
    assert(s3.dependentCount() == 0)
    assert(s3.dependOnCount() == 0)
  }

  @Test def varsAndSignalsCanHaveDependencies() {
    val v1 = Var(0)
    val v2 = Var(2)
    val v3 = Var(true)
    val s = Signal { if (v3()) v1() else v2() }
    assert(v1.dependentCount() == 1)
    assert(v2.dependentCount() == 0)
    assert(v3.dependentCount() == 1)
    assert(s.dependentCount() == 0)
    assert(s.dependOnCount() == 2)
  }

  @Test def explicitReevaluationDoesNotChangeDependencies() {
    val v1 = Var(0)
    val v2 = Var(2)
    val v3 = Var(true)
    val s = Signal { if (v3()) v1() else v2() }
    assert(v1.dependentCount() == 1)
    assert(v2.dependentCount() == 0)
    assert(v3.dependentCount() == 1)
    assert(s.dependentCount() == 0)
    assert(s.dependOnCount() == 2)

    s.triggerReevaluation()
    assert(v1.dependentCount() == 1)
    assert(v2.dependentCount() == 0)
    assert(v3.dependentCount() == 1)
    assert(s.dependentCount() == 0)
    assert(s.dependOnCount() == 2)
  }

  @Test def implicitReevaluationChangesDependenciesCorrectly() {
    val v1 = Var(0)
    val v2 = Var(2)
    val v3 = Var(true)
    val s = Signal { if (v3()) v1() else v2() }
    assert(v1.dependentCount() == 1)
    assert(v2.dependentCount() == 0)
    assert(v3.dependentCount() == 1)
    assert(s.dependentCount() == 0)
    assert(s.dependOnCount() == 2)

    v3.set(false)
    assert(v1.dependentCount() == 0)
    assert(v2.dependentCount() == 1)
    assert(v3.dependentCount() == 1)
    assert(s.dependentCount() == 0)
    assert(s.dependOnCount() == 2)
  }

  @Test def signalsCanHaveDependents() {
    val s1 = Var { 5 }
    val s2 = Signal(10)
    val s3 = Signal { s1() + s2() }
    assert(s1.dependentCount() == 1)
    assert(s2.dependentCount() == 1)
    assert(s2.dependOnCount() == 0)
    assert(s3.dependentCount() == 0)
    assert(s3.dependOnCount() == 2)
  }

  @Test def fibonacciDependencies() {
    val f1 = Var(1)
    val f2 = Var(1)
    val f3 = Signal { f1() + f2() }
    val f4 = Signal { f2() + f3() }
    assert(f1.dependentCount() == 1)
    assert(f2.dependentCount() == 2)
    assert(f3.dependentCount() == 1)
    assert(f3.dependOnCount() == 2)
    assert(f4.dependentCount() == 0)
    assert(f4.dependOnCount() == 2)
  }

}
