package test


import org.junit.Before
import org.junit.After
import org.junit.Test
import org.mockito.Mockito.verify
import org.mockito.Mockito.times
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar


import react.Signal
import react.DepHolder
import react.Var
import react.Handler
import react._



class TimestampTest extends AssertionsForJUnit with MockitoSugar {
  
  @Before def initialize() {
    TS.reset      
  }
  @After def cleanup() {
    TS.reset      
  }
  
  @Test def timeStampsStartWithZeroRoundNumberAndZeroSequenceNumber =  {
    assert( TS.newTs equals new Stamp(0,0) )
  }
  
  @Test def requestingSeveralTimeStampsIncereasesSequenceNumber =  {
    TS.newTs
    TS.newTs
    TS.newTs
    assert( TS.newTs equals new Stamp(0,3) )
  }
  
  @Test def nextRoundRestarstRoundNumber =  {
    TS.newTs
    TS.newTs
    TS.nextRound
    assert(TS.newTs match { case Stamp(_,0) => true })
  }
 
  @Test def nextRoundIncreasesSequenceNumber =  {
    TS.newTs
    TS.newTs
    TS.nextRound
    assert(TS.newTs match { case Stamp(1,_) => true })
  } 
}












