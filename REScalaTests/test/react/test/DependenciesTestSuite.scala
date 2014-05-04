package react.test

import org.junit.Before
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar

import react._
import `macro`.SignalMacro.{ SignalM => Signal }
import react.events._

class DependenciesTestSuite extends AssertionsForJUnit with MockitoSugar {

  @Before def initialize() {}

  @Test def varsDoNotHaveDependentsByDefault() {
    val v1 = Var(0)
    val v2 = Var("Hello World")
    val v3 = Var(false)
    assert(v1.dependents.size == 0)
    assert(v2.dependents.size == 0)
    assert(v3.dependents.size == 0)
  }

  @Test def signalsDoNotHaveDependenciesByDefault() {
    val s1 = Signal { 0 }
    val s2 = Signal { "Hello World" }
    val s3 = Signal { false }
    assert(s1.dependents.size == 0)
    assert(s1.dependOn.size == 0)
    assert(s2.dependents.size == 0)
    assert(s2.dependOn.size == 0)
    assert(s3.dependents.size == 0)
    assert(s3.dependOn.size == 0)
  }

  @Test def varsAndSignalsCanHaveDependencies() {
    val v1 = Var(0)
    val v2 = Var(2)
    val v3 = Var(true)
    val s = Signal { if (v3()) v1() else v2() }
    assert(v1.dependents.size == 1)
    assert(v2.dependents.size == 0)
    assert(v3.dependents.size == 1)
    assert(s.dependents.size == 0)
    assert(s.dependOn.size == 2)
  }

  @Test def explicitReevaluationDoesNotChangeDependencies() {
    val v1 = Var(0)
    val v2 = Var(2)
    val v3 = Var(true)
    val s = Signal { if (v3()) v1() else v2() }
    assert(v1.dependents.size == 1)
    assert(v2.dependents.size == 0)
    assert(v3.dependents.size == 1)
    assert(s.dependents.size == 0)
    assert(s.dependOn.size == 2)

    s.triggerReevaluation()
    assert(v1.dependents.size == 1)
    assert(v2.dependents.size == 0)
    assert(v3.dependents.size == 1)
    assert(s.dependents.size == 0)
    assert(s.dependOn.size == 2)
  }

  @Test def implicitReevaluationChangesDependenciesCorrectly() {
    val v1 = Var(0)
    val v2 = Var(2)
    val v3 = Var(true)
    val s = Signal { if (v3()) v1() else v2() }
    assert(v1.dependents.size == 1)
    assert(v2.dependents.size == 0)
    assert(v3.dependents.size == 1)
    assert(s.dependents.size == 0)
    assert(s.dependOn.size == 2)

    v3.setVal(false)
    assert(v1.dependents.size == 0)
    assert(v2.dependents.size == 1)
    assert(v3.dependents.size == 1)
    assert(s.dependents.size == 0)
    // TODO why is s.dependOn.size 3!?
    assert(s.dependOn.size == 2)
  }

  @Test def signalsCanHaveDependents() {
    val s1 = Var { 5 }
    val s2 = Signal(10)
    val s3 = Signal { s1() + s2() }
    assert(s1.dependents.size == 1)
    assert(s2.dependents.size == 1)
    assert(s2.dependOn.size == 0)
    assert(s3.dependents.size == 0)
    assert(s3.dependOn.size == 2)
  }

  @Test def fibonacciDependencies() {
    val f1 = Var(1)
    val f2 = Var(1)
    val f3 = Signal { f1() + f2() }
    val f4 = Signal { f2() + f3() }
    assert(f1.dependents.size == 1)
    assert(f2.dependents.size == 2)
    assert(f3.dependents.size == 1)
    assert(f3.dependOn.size == 2)
    assert(f4.dependents.size == 0)
    assert(f4.dependOn.size == 2)
  }

}
