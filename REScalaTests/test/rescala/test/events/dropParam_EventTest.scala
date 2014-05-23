package rescala.test.events


import org.junit.After
import org.junit.Before
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.events._
import rescala._




class dropParam_EventTest extends AssertionsForJUnit with MockitoSugar {


  @Before def initialize() {
    TS.reset
  }
  @After def cleanup() {
    TS.reset
  }

  @Test def handlerOf_dropParam_IsExecuted() = {
    var test = 0
    val e1 = new ImperativeEvent[Int]()
    val e1_drop: EventNode[Unit] = e1.dropParam
    e1_drop += ((x)=>{ test += 1; })

    e1(10)
    e1(10)
    assert(test == 2)
  }

}
