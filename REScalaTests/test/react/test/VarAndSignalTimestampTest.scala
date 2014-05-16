package react.test


import scala.collection.mutable.ListBuffer
import org.junit.Before
import org.junit.After
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import react.Var
import react._



class VarAndSignalTimestampTest extends AssertionsForJUnit with MockitoSugar {

  @Before def initialize() {
    TS.reset
  }
  @After def cleanup() {
    TS.reset
  }

  @Test def xxx() =  {
    val v = Var(10)
    v.set(11)
    assert( v.timestamps equals ListBuffer(Stamp(1,0)) )
  }

  @Test def xxxx() =  {
    val v = Var(10)
    val s = StaticSignal(v){ v.get + 1 }
    v.set(11)
    assert( v.timestamps equals ListBuffer(Stamp(1,0)) )
    assert( s.timestamps equals ListBuffer(Stamp(1,1)) )
  }

}
