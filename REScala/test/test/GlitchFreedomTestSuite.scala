package test


import org.junit.After
import org.junit.Before
import org.junit.Test
import org.mockito.Mockito.verify
import org.mockito.Mockito.times
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar

import scala.collection.immutable.Set


import react.Signal
import react.DepHolder
import react.Var
import react.Handler
import react._



class GlitchFreedomTestSuite extends AssertionsForJUnit with MockitoSugar {
  
  
  class SuperInt(n: Int){ def in(s: Set[Int]) = s.contains(n) }
  implicit def superInt(n: Int) = new SuperInt(n: Int) 
  
  
  var v1: Var[Int] = _
  var v2: Var[Int] = _
  var v3: Var[Int] = _
  var s1: Signal[Int] = _
  var s2: Signal[Int] = _
  var s3: Signal[Int] = _
  


  @Before def initialize() {
    TS.reset      
  }
  @After def cleanup() {
    TS.reset    
  }

  @Test def noGlitchesInSimpleCase() = {
    
    v1 = Var(1)
    s1 = Signal(v1) { 2 * v1.getValue }
    s2 = Signal(v1) { 3 * v1.getValue }
    s3 = Signal(s1, s2) { s1.getVal + s2.getVal }

    v1.setVal(3)

    assert(v1.timestamps.toList match { case List(Stamp(1, 0)) => true })
    assert(s1.timestamps.toList match { case List(Stamp(1, x)) if x in Set(1, 2) => true })
    assert(s2.timestamps.toList match { case List(Stamp(1, x)) if x in Set(1, 2) => true })
    assert(s3.timestamps.toList match { case List(Stamp(1, 3)) => true })

  }
  
  
   @Test def noGlitches() =  {
    
    v1 = Var(1)
    
    s1 = Signal(v1){ 2 * v1.getValue }
    s2 = Signal(v1){ 3 * v1.getValue }
    s3 = Signal(s1,s2){ s1.getVal + s2.getVal }

    
    v1.setVal(3)

    assert(v1.timestamps.toList match { case List(Stamp(1,0)) => true })
    assert(s1.timestamps.toList match { case List(Stamp(1,x)) if x in Set(1,2) => true })
    assert(s2.timestamps.toList match { case List(Stamp(1,x)) if x in Set(1,2) => true })
    assert(s3.timestamps.toList match { case List(Stamp(1,3)) => true })

  }


  
  
  

  
}












