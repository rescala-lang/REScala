package react.test


import react.Handler
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import react._


class SignalSyntTestSuite extends AssertionsForJUnit with MockitoSugar {

  @Test def signalReEvaluatesTheExpressionWhenSomethingItDependsOnIsUpdated() {
    val v  = VarSynt(0)
    var i = 1
    var s: SignalSynt[Int] = SignalSynt[Int](v){s=> v(s) + i }
    i = 2
    v.setValue(2)
    assert(s.get == 4)
  }

  @Test def theExpressionIsNoteEvaluatedEveryTimeGetValIsCalled() {
    var a = 10
    val s: SignalSynt[Int] = SignalSynt[Int](List())(s=> 1 + 1 + a )
    assert(s.get === 12)
    a = 11
    assert(s.get === 12)
  }


  @Test def simpleSignalReturnsCorrectExpressions() {
    var s: SignalSynt[Int] = SignalSynt[Int](List())(s=> 1 + 1 + 1 )
    assert(s.get === 3)
  }

  @Test def theExpressionIsEvaluatedOnlyOnce() {

    var a = 0
    val v = VarSynt(10)
    var s1: SignalSynt[Int] = SignalSynt[Int](v){s=> a +=1; v(s) % 10 }
    var s2: SignalSynt[Int] = SignalSynt[Int](s1){s=> a }


    assert(a == 1)
    v.setValue(11)
    assert(a == 2)
    v.setValue(21)
    assert(a == 3)
  }

  @Test def handlersAreExecuted() =  {

    var test = 0
    val v = VarSynt(1)

    val s1 = SignalSynt[Int](v){s=> 2 * v(s) }
    val s2 = SignalSynt[Int](v){s=> 3 * v(s) }
    val s3 = SignalSynt[Int](s1,s2){s=> s1(s) + s2(s) }

    s1 addDependent Handler{ test += 1 }
    s2 addDependent Handler{ test += 1 }
    s3 addDependent Handler{ test += 1 }

    assert(test == 0)

    v.setValue(3)
    assert(test == 3)

  }

  @Test def levelIsCorrectlyComputed() =  {

    var test = 0
    val v = VarSynt(1)

    val s1 = SignalSynt[Int](v){s=> 2 * v.get }
    val s2 = SignalSynt[Int](v){s=> 3 * v.get }
    val s3 = SignalSynt[Int](s1,s2){s=> s1.get + s2.get }

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
    assert(s.get == 0)
    v2.setValue(1)
    assert(i == 2)
    assert(s.get == 1)
    v3.setValue(11) // No effect
    assert(i == 2)
    assert(s.get == 1)

    v1.setValue(false)
    assert(i == 3)
    assert(s.get == 11)
    v3.setValue(12)
    assert(i == 4)
    assert(s.get == 12)
    v2.setValue(2) // No effect
    assert(i == 4)
    assert(s.get == 12)
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
    v2.setValue(1)
    assert(test == 1)

    v1.setValue(false)
    assert(test == 2)
    v3.setValue(11)
    assert(test == 3)

    v2.setValue(2)
    assert(test == 3)

    v1.setValue(true)
    assert(test == 4)
    v2.setValue(3)
    assert(test == 5)


  }

   @Test def dependantIsOnlyInvokedOnValueChanges() {
    var changes = 0
    val v = VarSynt(1)
    val s: SignalSynt[Int] = SignalSynt[Int]{ s: SignalSynt[Int]=>
      changes += 1; v(s) + 1 }
    assert(changes == 1)
    assert(s.get == 2)
    v.setValue(2)
    assert(changes == 2)
    v.setValue(2)
    assert(changes == 2) // is actually 3
  }

}
