package react.test


import org.junit.Before
import org.junit.After
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import react._



class TimestampTest extends AssertionsForJUnit with MockitoSugar {

  @Before def initialize() {
    TS.reset
  }
  @After def cleanup() {
    TS.reset
  }

  @Test def timeStampsStartWithZeroRoundNumberAndZeroSequenceNumber() =  {
    assert( TS.newTs equals new Stamp(0,0) )
  }

  @Test def requestingSeveralTimeStampsIncereasesSequenceNumber() =  {
    TS.newTs
    TS.newTs
    TS.newTs
    assert( TS.newTs equals new Stamp(0,3) )
  }

  @Test def nextRoundRestarstRoundNumber() =  {
    TS.newTs
    TS.newTs
    TS.nextRound
    assert(TS.newTs match { case Stamp(_,0) => true; case _ => false })
  }

  @Test def nextRoundIncreasesSequenceNumber() =  {
    TS.newTs
    TS.newTs
    TS.nextRound
    assert(TS.newTs match { case Stamp(1,_) => true; case _ => false })
  }
}
