package rescala.pipelining.tests

import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.pipelining.PipelineEngine
import rescala.Var
import org.junit.Test
import rescala.Signals
import rescala.graph.Framed
import scala.collection.immutable.Queue
import rescala.turns.Turn
import rescala.pipelining.tests.PipelineTestUtils._

class ConflictResolvingTest extends AssertionsForJUnit with MockitoSugar {
  
  implicit val engine = new PipelineEngine
  
  /*
   * This test suite runs on the following topology: S1 and S2 are sources
   * and D1 and D2 are dependencies
   * 
   * S1    S2
   * | \  / |
   * |  \/  |
   * |  /\  |
   * | /  \ |
   * vv    vv
   * D1    D2 
   */
  
  val s1 = Var(0)
  val s2 = Var(0)
  val d1 = Signals.lift(s1, s2) { (v1, v2) => randomWait{v1 - v2}}
  val d2 = Signals.lift(s1, s2) { (v1, v2) => randomWait{v1 - 2 * v2}}
  
  // In the following the propagation is done by hand and not by the queue
  // This allows to fix a pseudo paralellismus of updates by invoking the
  // reevaluate methods in a fix order
  
  @Test
  def testSingleUpdate() = {
     val turn1 = engine.makeTurn
     
     // Create frames for all reachable reactives
     engine.createFrame(turn1, s1)
     engine.createFrame(turn1, d1)
     engine.createFrame(turn1, d2)
     
     assert(s1.getPipelineFrames().size == 1)
     assert(d1.getPipelineFrames().size == 1)
     assert(d2.getPipelineFrames().size == 1)
  }
  
  @Test
  def testMultipleNonConflictingUpdates() = {
    val turn1 = engine.makeTurn
    val turn2 = engine.makeTurn
    
    engine.createFrame(turn1, s1)
    engine.createFrame(turn2, s2)
    engine.createFrame(turn1, d1)
    assert(frameTurns(d1) == Queue(turn1))
    engine.createFrame(turn2, d1)
    assert(frameTurns(d1) == Queue(turn1, turn2))
    engine.createFrame(turn1, d2)
    engine.createFrame(turn2, d2)
    assert(frameTurns(d2) == Queue(turn1, turn2))
    assert(frameTurns(d1) == Queue(turn1, turn2))
  }
  
  @Test
  def testConflictingUpdates() = {
    val turn1 = engine.makeTurn
    val turn2 = engine.makeTurn
    
    engine.createFrame(turn1, s1)
    engine.createFrame(turn2, s2)
    engine.createFrame(turn1, d1)
    engine.createFrame(turn2, d2)
    engine.createFrame(turn2, d1)
    assert(frameTurns(d1) == Queue(turn1, turn2))
    assert(frameTurns(d2) == Queue(turn2))
    
    // Now the interesting part: putting turn1 on d2 creates a cycle
    // Which the engine should resolve by moving turn1 back in d1
    engine.createFrame(turn1, d2) 
    assert(frameTurns(d1) == Queue(turn2, turn1))
    assert(frameTurns(d2) == Queue(turn2, turn1))
  }
  
    @Test
  def testConflictingUpdates2() = {
    val turn1 = engine.makeTurn
    val turn2 = engine.makeTurn
    
    engine.createFrame(turn1, s1)
    engine.createFrame(turn2, s2)
    engine.createFrame(turn2, d1)
    engine.createFrame(turn1, d1)
    engine.createFrame(turn2, d2)
    assert(frameTurns(d1) == Queue(turn2, turn1))
    assert(frameTurns(d2) == Queue(turn2))
    
    // Now the interesting part: putting turn1 on d2 creates a cycle
    // Which the engine should resolve by moving turn1 back in d1
    engine.createFrame(turn1, d2) 
    assert(frameTurns(d1) == Queue(turn2, turn1))
    assert(frameTurns(d2) == Queue(turn2, turn1))
  }
  
  @Test
  def testEvaluationSequential() = {
    // Everything sequential => there cannot be any conflict
    assert (d1.now == 0)
    assert (d2.now == 0)
    s1.set(10)
    assert(d1.now == 10)
    assert(d2.now == 10)
    s2.set(5)
    assert(d1.now == 5)
    assert(d2.now == 0)
  }
  
  @Test
  def testEvaluationParallel() = {
    for (i <- 1 to 100) {
      val update1 =createThread{ 
        s1.set(10)
      }
      val update2 = createThread{
        s2.set(5)
      }
      
      if (i % 2 == 0) {
        update1.start
        update2.start
      } else {
        update2.start
        update1.start
      }
      
      update1.join
      update2.join
      
      assert(s1.getPipelineFrames().isEmpty)
      assert(s2.getPipelineFrames().isEmpty)
      assert(d1.getPipelineFrames().isEmpty)
      assert(d2.getPipelineFrames().isEmpty)
      
      // Now either update1 was scheduled first or update2
      // Independent of the if statement above
      
      assert(d1.now == 5)
      assert(d2.now == 0)
      
      s1.set(0)
      s2.set(0)
    }
  }
  

}