package rescala.test

//These 3 are for JUnitRunner
import org.junit.Before
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.Signal
import rescala._

class VarTestSuite extends AssertionsForJUnit with MockitoSugar {

  var v1: Var[Int] = _
  var v2: Var[Int] = _
  var s1: Signal[Int] = _
  var s2: Signal[Int] = _
  var s3: Signal[Int] = _

  @Before def initialize() {

    v1 = Var(1)
    v2 = Var(2)
    s1 = mock[Signal[Int]]
    s2 = mock[Signal[Int]]
    s3 = mock[Signal[Int]]
  }

  @Test def getValAfterCreationReturnsInitializationValue() {
    val v = Var(1)
    assert(v.get == 1)
  }

  @Test def getValReturnsCorrectValue() {
    val v = Var(1)
    v.set(10)
    assert(v.get == 10)
  }


  @Test def varNotifiesSignalOfChanges() {
    val v = Var(1)
    val s = StaticSignal(List(v)){ v.get + 1 }
    assert(v.get == 1)

    assert(s.get == 2)
    v.set(2)
    assert(v.get == 2)
    assert(s.get == 3)

  }

  @Test def changeEventOnlyTriggeredOnValueChange() {
    var changes = 0
    val v = Var(1)
    val changed = StaticSignal(List(v)){ v.get }.change
    changed += {_ => changes += 1}

    v.set(2)
    assert(changes == 1)
    v.set(3)
    assert(changes == 2)
    v.set(3)
    assert(changes == 2)
  }

  @Test def dependantIsOnlyInvokedOnValueChange() {
    var changes = 0
    val v = Var(1)
    val s = StaticSignal(List(v)){ changes += 1; v.get + 1 }
    assert(s.get == 2)
    v.set(2)
    assert(changes == 2)
    v.set(2)
    assert(changes == 2)
  }


}
