package rescala.test.events


import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.Implicits.default
import rescala.events._


class dropParam_EventTest extends AssertionsForJUnit with MockitoSugar {

  @Test def handlerOf_dropParam_IsExecuted() = {
    var test = 0
    val e1 = new ImperativeEvent[Int]()
    val e1_drop: Event[Unit] = e1.dropParam
    e1_drop += ((x) => { test += 1; })

    e1(10)
    e1(10)
    assert(test == 2)
  }

}
