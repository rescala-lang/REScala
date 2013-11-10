package react.test.events


import org.junit.After
import org.junit.Before
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import react.events._
import react._




class PredicateEventTest extends AssertionsForJUnit with MockitoSugar {
  

  @Before def initialize() {
    TS.reset      
  }
  @After def cleanup() {
    TS.reset    
  }

  @Test def predicateEventIsExecutedOnlyIfThePredicateIsTrue = {
    var test = 0
    var cond = false
    val e1 = new ImperativeEvent[Int]()
    val e2 = e1 && ((x: Int)=>cond)
    e2 += ( (x: Int) => { test += 1 })
    
    e1(10)
    e1(10)
    e1(10)
    assert(test == 0)
    cond = true
    e1(10)
    //e1(10)
    //assert(test == 2)
  }
  
  
  
}












