package test.events


import org.junit.After
import org.junit.Before
import org.junit.Test
import org.mockito.Mockito.verify
import org.mockito.Mockito.times
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar

import scala.collection.immutable.Set


import react._
import react.events._
//import eevents.lib._



class EventTest extends AssertionsForJUnit with MockitoSugar {
  

  @Before def initialize() {
    TS.reset      
  }
  @After def cleanup() {
    TS.reset    
  }

  @Test def handlersAreExecuted = {
    var test = 0
    val e1 = new ImperativeEvent[Int]()
    e1 += ( (x: Int) => { test += 1 })
    e1(10)
    e1(10)
    assert(test == 2)
  }
  
  @Test def eventHandlersCanBeRemoved = {
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
  
  @Test def correctValueIsReceived = {
    var test = 0
    val e1 = new ImperativeEvent[Int]()
    e1 += ( (x: Int) => { test += x })
    e1(10)
    assert(test == 10)
  }
  
  @Test def eventsWithoutParamsIsCalled = {
    var test = 0
    val e1 = new ImperativeEvent[Unit]()
    e1 += ( _ => { test += 1 })
    e1()
    assert(test == 1)
  }
 
  
}












