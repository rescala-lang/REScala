package react.test


import react.Reactive
import org.junit.Before
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar



class ReactiveTest extends AssertionsForJUnit with MockitoSugar {
  
  
  var r1: Reactive = _
  var r2: Reactive = _
   

  @Before def initialize() {
    
    r1 = new {} with Reactive {}
    r2 = new {} with Reactive {}
    
  }

  @Test def reactivesAreOrderedBasedOnTheLevel() {
    r1.level = 5
    r2.level = 6
    assert(r1 > r2) // priority, NOT level
  }
  
}












