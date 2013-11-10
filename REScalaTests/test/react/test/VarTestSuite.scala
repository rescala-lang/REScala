package react.test

//These 3 are for JUnitRunner
import org.junit.Before
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import react.Signal
import react._

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
    assert(v.getValue == 1)
  }
  
  @Test def getValReturnsCorrectValue() {
    val v = Var(1)
    v.setVal(10)
    assert(v.getValue == 10)
  }
  
  
  @Test def varNotifiesSignalOfChanges() {
    val v = Var(1)
    val s = StaticSignal(List(v)){ v.getValue + 1 }
    assert(v.getValue == 1)

    assert(s.getVal == 2)
    v.setVal(2)
    assert(v.getValue == 2)
    assert(s.getVal == 3)

  }
  
  @Test def changeEventOnlyTriggeredOnValueChange() {
    var changes = 0
    val v = Var(1)
    val changed = StaticSignal(List(v)){ v.getValue }.change
    changed += {_ => changes += 1}

    v.setVal(2)
    assert(changes == 1)
    v.setVal(3)
    assert(changes == 2)
    v.setVal(3)
    assert(changes == 2)
  }
  
  @Test def dependantIsOnlyInvokedOnValueChange() {
    var changes = 0
    val v = Var(1)
    val s = StaticSignal(List(v)){ changes += 1; v.getValue + 1 }
    assert(s.getVal == 2)
    v.setVal(2)
    assert(changes == 2)
    v.setVal(2)
    assert(changes == 2)
  }


}












