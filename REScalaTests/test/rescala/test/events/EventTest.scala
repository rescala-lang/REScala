package rescala.test.events


import org.junit.After
import org.junit.Before
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala._
import rescala.events._
//import eevents.lib._



class EventTest extends AssertionsForJUnit with MockitoSugar {


  @Before def initialize() {
    TS.reset()
  }
  @After def cleanup() {
    TS.reset()
  }

  @Test def handlersAreExecuted() = {
    var test = 0
    val e1 = new ImperativeEvent[Int]()
    e1 += ( (x: Int) => { test += 1 })
    e1(10)
    e1(10)
    assert(test == 2)
  }

  @Test def eventHandlersCanBeRemoved() = {
    var test = 0
    val e1 = new ImperativeEvent[Int]()
    val f = ( (x: Int) => { test += 1 })
    e1 += f
    e1(10)
    e1(10)
    e1 -= f
    e1(10)
    assert(test == 2)
  }

  @Test def correctValueIsReceived() = {
    var test = 0
    val e1 = new ImperativeEvent[Int]()
    e1 += ( (x: Int) => { test += x })
    e1(10)
    assert(test == 10)
  }

  @Test def eventsWithoutParamsIsCalled() = {
    var test = 0
    val e1 = new ImperativeEvent[Unit]()
    e1 += ( _ => { test += 1 })
    e1(())
    assert(test == 1)
  }


   @Test def functionIsCalled() = {
    var test = 0

    def f(x: Int) { test += 1 }

    val e1 = new ImperativeEvent[Int]()
    e1 += f _

    e1(10)
    e1(10)
    assert(test == 2)
  }

   /*
    * Fails because the queue does not support repetitions
    * so one of the two handlers is removed. Refactor to a queue that
    * supports repetitions, but this requires to change the implementation
    * of the or event that assumes no repetitions in the queue.
    */
  @Test def XXXfails() = {

    var test = 0
    val eventA = new ImperativeEvent[Unit]
    val eventB = new ImperativeEvent[Int]

    eventB += ( _ => { test += 1 })

    //the following prints only 2
    eventA += (_ => eventB(1))
    eventA += (_ => eventB(2))

    //whereas this executes both:
    //eventA += (_ => println(1))
    //eventA += (_ => println(2))

    eventA(())

    assert(test == 2)

  }


  @Test def eventsWithMethodHandlersWithNoParameters() = {


    import rescala.conversions.EventConversions.emptyToUnit

    var test = 0
    val e = new ImperativeEvent[Unit]
    def m1() = { test += 1 }

    e += m1 _
    e(())
    e(())
    assert(test == 2)

  }

  @Test def eventsWithMethodHandlersWithParameter() = {

    var test = 0
    val e = new ImperativeEvent[Int]
    def m1(x: Int) = { test += 1 }

    e += m1 _
    e(10)
    e(10)
    assert(test == 2)

  }

}
