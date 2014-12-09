package tests.rescala

//These 3 are for JUnitRunner

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.graph.Engines
import Engines.default
import rescala.Var


class VarTestSuite extends AssertionsForJUnit with MockitoSugar {

  @Test def getValAfterCreationReturnsInitializationValue(): Unit = {
    val v = Var(1)
    assert(v.now == 1)
  }

  @Test def getValReturnsCorrectValue(): Unit = {
    val v = Var(1)
    v.set(10)
    assert(v.now == 10)
  }


  @Test def varNotifiesSignalOfChanges(): Unit = {
    val v = Var(1)
    val s = v.map { _ + 1 }
    assert(v.now == 1)

    assert(s.now == 2)
    v.set(2)
    assert(v.now == 2)
    assert(s.now == 3)

  }

  @Test def changeEventOnlyTriggeredOnValueChange(): Unit = {
    var changes = 0
    val v = Var(1)
    val changed = v.change
    changed += { _ => changes += 1 }

    v.set(2)
    assert(changes == 1)
    v.set(3)
    assert(changes == 2)
    v.set(3)
    assert(changes == 2)
  }

  @Test def dependantIsOnlyInvokedOnValueChange(): Unit = {
    var changes = 0
    val v = Var(1)
    val s = v.map { i => changes += 1; i + 1 }
    assert(s.now == 2)
    v.set(2)
    assert(changes == 2)
    v.set(2)
    assert(changes == 2)
  }


}
