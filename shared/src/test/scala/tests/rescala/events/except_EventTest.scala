package tests.rescala.events


import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.Evt
import rescala.turns.{Engine, Turn}
import tests.rescala.JUnitParameters

object except_EventTest extends JUnitParameters

@RunWith(value = classOf[Parameterized])
class except_EventTest(engine: Engine[Turn]) extends AssertionsForJUnit with MockitoSugar {
  implicit val implicitEngine: Engine[Turn] = engine

  @Test def handlerOf_except_IsExecutedIfBasicEventFires(): Unit = {
    var test = 0
    val e1 = Evt[Int]()
    val e2 = Evt[Int]()
    val e1_except_e2 = e1 \ e2
    e1_except_e2 += ((x: Int) => { test += 1 })

    e1(10)
    assert(test == 1)

  }

  @Test def handlerOf_except_IgnoresTheSecondEventIfFires(): Unit = {
    var test = 0
    val e1 = Evt[Int]()
    val e2 = Evt[Int]()
    val e1_except_e2 = e1 \ e2
    e1_except_e2 += ((x: Int) => { test += 1 })

    e2(10)
    assert(test == 0)

  }

  @Test def handlerOf_except_IsExecutedOnlyIfFirstEventFiresAndNotTheSecond(): Unit = {

    var test = 0

    var cond = false
    val e1 = Evt[Int]()
    val e2 = e1 map ((x: Int) => x * 2)
    val e3 = e1 && (_ => cond)
    val e1_except_e2 = e2 \ e3
    e1_except_e2 += ((x: Int) => { test += 1 })


    e1(10)
    assert(test == 1)

    cond = true
    e1(10)
    assert(test == 1)

    cond = false
    e1(10)
    assert(test == 2)

  }


  @Test def handlerOf_except_GetsTheCorrectValue(): Unit = {

    var value = 0

    var cond = false
    val e1 = Evt[Int]()
    val e2 = e1 map ((x: Int) => x)
    val e3 = (e1 map ((x: Int) => x * 2)) && (_ => cond)
    val e1_except_e2 = e2 \ e3
    e1_except_e2 += ((x: Int) => { value = x })


    e1(10)
    assert(value == 10)

    cond = true
    e1(11)
    assert(value == 10)

    cond = false
    e1(12)
    assert(value == 12)

  }

}
