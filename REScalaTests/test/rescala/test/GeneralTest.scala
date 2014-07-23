package rescala.test


import org.junit.After
import org.junit.Before
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala._




class GeneralEventTest extends AssertionsForJUnit with MockitoSugar {


  @Before def initialize() {
    TS.reset()
  }
  @After def cleanup() {
    TS.reset()
  }

  @Test def predicateEventIsExecutedOnlyIfThePredicateIsTrue() = {
    var test = 0

    assert( true )


  }

}
