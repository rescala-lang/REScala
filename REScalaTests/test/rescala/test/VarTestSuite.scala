package rescala.test

//These 3 are for JUnitRunner
import org.junit.{Before, Test}
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.signals._

class VarTestSuite extends AssertionsForJUnit with MockitoSugar {

  var v1: Var[Int] = _
  var v2: Var[Int] = _
  var s1: Signal[Int] = _
  var s2: Signal[Int] = _
  var s3: Signal[Int] = _

  @Before def initialize(): Unit = {

    v1 = Var(1)
    v2 = Var(2)
    s1 = mock[Signal[Int]]
    s2 = mock[Signal[Int]]
    s3 = mock[Signal[Int]]
  }

  @Test def getValAfterCreationReturnsInitializationValue(): Unit = {
    val v = Var(1)
    assert(v.get == 1)
  }

  @Test def getValReturnsCorrectValue(): Unit = {
    val v = Var(1)
    v.set(10)
    assert(v.get == 10)
  }


  @Test def varNotifiesSignalOfChanges(): Unit = {
    val v = Var(1)
    val s = StaticSignal(List(v)){ v.get + 1 }
    assert(v.get == 1)

    assert(s.get == 2)
    v.set(2)
    assert(v.get == 2)
    assert(s.get == 3)

  }

  @Test def changeEventOnlyTriggeredOnValueChange(): Unit = {
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

  @Test def dependantIsOnlyInvokedOnValueChange(): Unit = {
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
