package rescala.test


//These 3 are for JUnitRunner
import org.junit.{Before, Test}
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala._

class SignalsAndVarsTestSuite extends AssertionsForJUnit with MockitoSugar {


  var v1: Var[Int] = _
  var v2: Var[Int] = _
  var v3: Var[Int] = _
  var s1: Signal[Int] = _
  var s2: Signal[Int] = _
  var s3: Signal[Int] = _



  @Before def initialize(): Unit = {

  }

  @Test def handlerIsCalledWhenChangeOccurs() =  {

    var test = 0
    v1 = Var(1)
    v2 = Var(2)

    s1 = StaticSignal(List(v1,v2)){ v1.get + v2.get }
    s1.changed += { (_) => test += 1 }

    assert(s1.get == 3)
    assert(test == 0)

    v2.set(3)
    assert(s1.get == 4)
    assert(test == 1)

    v2.set(3)
    assert(s1.get == 4)
    assert(test == 1)

  }




}
