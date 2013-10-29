package test


import react.Handler
import org.junit.Before
import org.junit.Test
import org.mockito.Mockito.verify
import org.mockito.Mockito.times
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar


import react._
import react.DepHolder
import react.Var

class SignalTestSuite extends AssertionsForJUnit with MockitoSugar {
  
  
  var dh: DepHolder = _
  var v:  Var[Int]  = _
  var s1: Signal[Int] = _
  var s2: Signal[Int] = _
  var s3: Signal[Int] = _
  


  @Before def initialize() {}

  @Test def dependencyHolderNotifiesDependentsWhenNotifyDependentsIsCalled() {
    
    dh = new {} with DepHolder {}
    v  = Var(0)
    s1 = mock[Signal[Int]]
    s2 = mock[Signal[Int]]
    s3 = mock[Signal[Int]]
    
    dh.addDependent(s1)
    dh.addDependent(s2)
    dh.addDependent(s3)
    dh.notifyDependents({})
    
    verify(s1).dependsOnchanged({},dh)
    verify(s2).dependsOnchanged({},dh)
    verify(s3).dependsOnchanged({},dh)

  }



  @Test def signalReEvaluatesTheExpression() {
    v  = Var(0)
    var i = 1
    var s: Signal[Int] = StaticSignal[Int](v) { i }
    i = 2
    v.setVal(2)
    assert(s.getValue == 2)
  }
  
  @Test def theExpressionIsNoteEvaluatedEveryTimeGetValIsCalled() {
    var a = 10
    var s: Signal[Int] = StaticSignal(List())( 1 + 1 + a )
    assert(s.getValue === 12)
    a = 11
    assert(s.getValue === 12)
  }
  

  @Test def simpleSignalReturnsCorrectExpressions() {
    var s: Signal[Int] = StaticSignal(List())( 1 + 1 + 1 )
    assert(s.getValue === 3)
  }

  @Test def theExpressionIsEvaluatedOnlyOnce() {
    
    var a = 0
    val v = Var(10)
    var s1: Signal[Int] = StaticSignal(v){ a +=1; v.getValue % 10 }
    var s2: Signal[Int] = StaticSignal(s1){ a }
    
    
    assert(a == 1)
    v.setVal(11)
    assert(a == 2)
    v.setVal(21)
    assert(a == 3)
  }
  
  @Test def handlersAreExecuted() =  {
    
    var test = 0
    v = Var(1)
    
    s1 = StaticSignal(v){ 2 * v.getValue }
    s2 = StaticSignal(v){ 3 * v.getValue }
    s3 = StaticSignal(s1,s2){ s1.getVal + s2.getVal }
      
    s1 addDependent Handler{ test += 1 }
    s2 addDependent Handler{ test += 1 }
    s3 addDependent Handler{ test += 1 }
    
    assert(test == 0)
    
    v.setVal(3)
    assert(test == 3)
 
  }
  
  @Test def levelIsCorrectlyComputed =  {
    
    var test = 0
    v = Var(1)
    
    s1 = StaticSignal(v){ 2 * v.getValue }
    s2 = StaticSignal(v){ 3 * v.getValue }
    s3 = StaticSignal(s1,s2){ s1.getVal + s2.getVal }
    
    assert(v.level == 0)
    assert(s1.level == 1)
    assert(s2.level == 1)
    assert(s3.level == 2)
    
 
  }
  
}












