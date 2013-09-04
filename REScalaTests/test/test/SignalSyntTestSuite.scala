package test


import react.Handler
import org.junit.Before
import org.junit.Test
import org.mockito.Mockito.verify
import org.mockito.Mockito.times
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar


import react._


class SignalSyntTestSuite extends AssertionsForJUnit with MockitoSugar {

  @Test def signalReEvaluatesTheExpressionWhenSomethingItDependsOnIsUpdated() {
    val v  = VarSynt(0)
    var i = 1
    var s: SignalSynt[Int] = SignalSynt[Int](v){s=> v(s) + i }
    i = 2
    v.setVal(2)
    assert(s.getVal == 4)
  }
  
  @Test def theExpressionIsNoteEvaluatedEveryTimeGetValIsCalled() {
    var a = 10
    val s: SignalSynt[Int] = SignalSynt[Int](List())(s=> 1 + 1 + a )
    assert(s.getVal === 12)
    a = 11
    assert(s.getVal === 12)
  }
  

  @Test def simpleSignalReturnsCorrectExpressions() {
    var s: SignalSynt[Int] = SignalSynt[Int](List())(s=> 1 + 1 + 1 )
    assert(s.getVal === 3)
  }

  @Test def theExpressionIsEvaluatedOnlyOnce() {
    
    var a = 0
    val v = VarSynt(10)
    var s1: SignalSynt[Int] = SignalSynt[Int](v){s=> a +=1; v(s) % 10 }
    var s2: SignalSynt[Int] = SignalSynt[Int](s1){s=> a }
    
    
    assert(a == 1)
    v.setVal(11)
    assert(a == 2)
    v.setVal(21)
    assert(a == 3)
  }
  
  @Test def handlersAreExecuted() =  {
    
    var test = 0
    val v = VarSynt(1)
    
    val s1 = SignalSynt[Int](v){s=> 2 * v(s) }
    val s2 = SignalSynt[Int](v){s=> 3 * v(s) }
    val s3 = SignalSynt[Int](s1,s2){s=> s1(s) + s2(s) }
      
    s1 += Handler{ test += 1 }
    s2 += Handler{ test += 1 }
    s3 += Handler{ test += 1 }
    
    assert(test == 0)
    
    v.setVal(3)
    assert(test == 3)
 
  }
  
  @Test def levelIsCorrectlyComputed =  {
    
    var test = 0
    val v = VarSynt(1)
    
    val s1 = SignalSynt[Int](v){s=> 2 * v.getVal }
    val s2 = SignalSynt[Int](v){s=> 3 * v.getVal }
    val s3 = SignalSynt[Int](s1,s2){s=> s1.getVal + s2.getVal }
    
    assert(v.level == 0)
    assert(s1.level == 1)
    assert(s2.level == 1)
    assert(s3.level == 2)
    
 
  }
  
  
/* Specific of SignalSynt */  
  
  
  @Test def signalDoesNotReEvaluateTheExpressionIfDependsOnIsUpdatedThatIsNotInCurrentDependencies() {
    val v1  = VarSynt(true)
    val v2  = VarSynt(0)
    val v3  = VarSynt(10)
    var i = 0
    val s: SignalSynt[Int] = SignalSynt[Int](v1,v2,v3){s=> 
      i+=1
      if(v1(s)) v2(s) else v3(s) 
    }
    
    assert(i == 1)
    assert(s.getVal == 0)
    v2.setVal(1)
    assert(i == 2)
    assert(s.getVal == 1)
    v3.setVal(11) // No effect
    assert(i == 2)
    assert(s.getVal == 1)
    
    v1.setVal(false)
    assert(i == 3)
    assert(s.getVal == 11)
    v3.setVal(12)
    assert(i == 4)
    assert(s.getVal == 12)
    v2.setVal(2) // No effect
    assert(i == 4)
    assert(s.getVal == 12)
  }

  
  
  @Test def keep_fixedDependencies() {

    val v1 = VarSynt(true)
    val v2 = VarSynt(0)
    val v3 = VarSynt(10)
    var i = 0
    var test = 0
    
    // val s: SignalSynt[Int] = SignalSynt[Int](v1, v2, v3) { s =>
    val s: SignalSynt[Int] = SignalSynt[Int]{ s: SignalSynt[Int]=>
      i += 1
      if (v1(s)) v2(s) else v3(s)
    }
    
    val e = s.change    
    e += ((x:(Int,Int))=>(test+=1))

    assert(test == 0)
    v2.setVal(1)
    assert(test == 1)
    
    v1.setVal(false)
    assert(test == 2)
    v3.setVal(11)
    assert(test == 3)
    
    v2.setVal(2)
    assert(test == 3)
    
    v1.setVal(true)
    assert(test == 4)
    v2.setVal(3)
    assert(test == 5)
    
    
  }
  
   @Test def dependantIsOnlyInvokedOnValueChanges() {
    var changes = 0
    val v = VarSynt(1)
    val s: SignalSynt[Int] = SignalSynt[Int]{ s: SignalSynt[Int]=>
      changes += 1; v(s) + 1 }
    assert(changes == 1)
    assert(s.getVal == 2)
    v.setVal(2)
    assert(changes == 2)
    v.setVal(2)
    assert(changes == 2) // is actually 3
  }
  
}












