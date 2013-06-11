package test


import org.junit.After
import org.junit.Before
import org.junit.Test
import org.mockito.Mockito.verify
import org.mockito.Mockito.times
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar

import scala.collection.immutable.Set



import react._




class GeneralEventTest extends AssertionsForJUnit with MockitoSugar {
  

  @Before def initialize() {
    TS.reset      
  }
  @After def cleanup() {
    TS.reset    
  }

  @Test def predicateEventIsExecutedOnlyIfThePredicateIsTrue = {
    var test = 0
    
    assert( true )
    
   
  }
  
}












