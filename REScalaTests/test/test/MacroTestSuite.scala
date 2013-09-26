package test




import react.Handler
import org.junit.Before
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import react.DepHolder
import react.VarSynt
import react.Var
import react._

import macro.SignalMacro.{SignalM => Signal}



class MacroTestSuite extends AssertionsForJUnit with MockitoSugar {
  
  
  var dh: DepHolder = _
  var v:  Var[Int]  = _
  var s1: Signal[Int] = _
  var s2: Signal[Int] = _
  var s3: Signal[Int] = _
  


  @Before def initialize() {}



  @Test def signalReEvaluatesTheExpression() {
    v  = VarSynt(0)
    var i = 1
    var s: Signal[Int] = Signal[Int]{ v() ; i }
    i = 2
    v.setVal(2)
    assert(s.getVal == 2)
  }
  
  @Test def theExpressionIsNoteEvaluatedEveryTimeGetValIsCalled() {
    var a = 10
    var s: Signal[Int] = Signal[Int]{ 1 + 1 + a }
    assert(s.getVal === 12)
    a = 11
    assert(s.getVal === 12)
  }
  

  @Test def simpleSignalReturnsCorrectExpressions() {
    var s: Signal[Int] = Signal( 1 + 1 + 1 )
    assert(s.getVal === 3)
  }

  @Test def theExpressionIsEvaluatedOnlyOnce() {
    
    var a = 0
    val v = VarSynt(10)
    var s1: Signal[Int] = Signal{ a +=1; v() % 10 }
    var s2: Signal[Int] = Signal{ s1; a }
    
    
    assert(a == 1)
    v.setVal(11)
    assert(a == 2)
    v.setVal(21)
    assert(a == 3)
  }
  
  @Test def handlersAreExecuted() =  {
    
    var test = 0
    v = VarSynt(1)
    
    s1 = Signal{ 2 * v() }
    s2 = Signal{ 3 * v() }
    s3 = Signal{ s1() + s2() }
      
    s1 addDependent Handler{ test += 1 }
    s2 addDependent Handler{ test += 1 }
    s3 addDependent Handler{ test += 1 }
    
    assert(test == 0)
    
    v.setVal(3)
    assert(test == 3)
 
  }
  
  @Test def levelIsCorrectlyComputed =  {
    
    var test = 0
    v = VarSynt(1)
    
    s1 = Signal{ 2 * v() }
    s2 = Signal{ 3 * v() }
    s3 = Signal{ s1() + s2() }
    
    s3.getVal
    
    assert(v.level == 0)
    assert(s1.level == 1)
    assert(s2.level == 1)
    assert(s3.level == 2)
    
 
  }


  
  
}












