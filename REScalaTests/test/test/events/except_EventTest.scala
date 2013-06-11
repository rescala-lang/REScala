package test.events


import org.junit.After
import org.junit.Before
import org.junit.Test
import org.mockito.Mockito.verify
import org.mockito.Mockito.times
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar

import scala.collection.immutable.Set


import react.events._
//import eevents.lib._
import react._




class except_EventTest extends AssertionsForJUnit with MockitoSugar {
  

  @Before def initialize() {
    TS.reset      
  }
  @After def cleanup() {
    TS.reset    
  }

  @Test def handlerOf_except_IsExecutedIfBasicEventFires = {
    var test = 0
    val e1 = new ImperativeEvent[Int]()
    val e2 = new ImperativeEvent[Int]()
    val e1_except_e2 = e1 \ e2
    e1_except_e2 += ( (x: Int) => { test += 1 })

    e1(10)
    assert(test == 1)
     
  }
  
  @Test def handlerOf_except_IgnoresTheSecondEventIfFires = {
    var test = 0
    val e1 = new ImperativeEvent[Int]()
    val e2 = new ImperativeEvent[Int]()
    val e1_except_e2 = e1 \ e2
    e1_except_e2 += ( (x: Int) => { test += 1 })

    e2(10)
    assert(test == 0)
     
  }
  
   @Test def handlerOf_except_IsExecutedOnlyIfFirstEventFiresAndNotTheSecond = {
    
    var test = 0
    
    var cond = false
    val e1 = new ImperativeEvent[Int]()
    val e2 = e1 map ((x: Int) => (x * 2))
    val e3 = e1 && cond
    val e1_except_e2 = e2 \ e3
    e1_except_e2 += ( (x: Int) => { test += 1 })
   
    
    e1(10)
    assert(test == 1)
    
    cond = true
    e1(10)
    assert(test == 1)
    
    cond = false
    e1(10)
    assert(test == 2)
 
  }
   
   
   @Test def handlerOf_except_GetsTheCorrectValue = {
    
    var value = 0
    
    var cond = false
    val e1 = new ImperativeEvent[Int]()
    val e2 = e1 map ((x: Int) => x)
    val e3 = ( e1 map ((x: Int) => (x * 2)) ) && cond
    val e1_except_e2 = e2 \ e3
    e1_except_e2 += ( (x: Int) => { value = x })
   
    
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












