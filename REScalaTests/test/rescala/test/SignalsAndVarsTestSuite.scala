package rescala.test


//These 3 are for JUnitRunner
import org.junit.{Before, Test}
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.signals._

class SignalsAndVarsTestSuite extends AssertionsForJUnit with MockitoSugar {


  @Test def handlerIsCalledWhenChangeOccurs() =  {

    var test = 0
    val v1 = Var(1)
    val v2 = Var(2)

    val s1 = Signals.lift(v1, v2) { _ + _ }
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
