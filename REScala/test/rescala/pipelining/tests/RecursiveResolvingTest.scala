package rescala.pipelining.tests

import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.Var
import rescala.pipelining.PipelineEngine
import rescala.Signals
import scala.collection.immutable.Queue
import org.junit.Test
import rescala.pipelining.tests.PipelineTestUtils._

class RecursiveResolvingTest extends AssertionsForJUnit with MockitoSugar {
  
  implicit var engine = new PipelineEngine
  
  /*
   *   S1    S2     S3
   *   | \  / | \  / |
   *   |  \/  |  \/  |
   *   |  /\  |  /\  |
   *   | /  \ | /  \ |
   *   VV    VVV    VV
   *   D1    D2     D3
   *    
   */
  
  val s1 = Var(0)
  val s2 = Var(0)
  val s3 = Var(0)
  
  val d1 = Signals.lift(s1,s2) {_ + _}
  val d2 = Signals.lift(s1, s2, s3) {_ + _ + _}
  val d3 = Signals.lift(s2, s3) {_ + _}
  
  @Test
  def resolveTransitiveConflict() = {
    val turn1 = engine.makeTurn
    val turn2 = engine.makeTurn
    val turn3 = engine.makeTurn
    
    engine.createFrame(turn3, d1)
    engine.createFrame(turn2, d2)
    engine.createFrame(turn1, d2)
    engine.createFrame(turn1, d3)
    engine.createFrame(turn2, d3)
    engine.createFrame(turn3, d3)
    
    assert(frameTurns(d1) == Queue(turn3))
    assert(frameTurns(d2) == Queue(turn1, turn2))
    assert(frameTurns(d3) == Queue(turn1, turn2, turn3))
    
    // Now put turn1 on d1, this creates a cycle:
    // At d1: turn1 -> turn3
    // At d2: turn2 -> turn1 and then
    // At d3: turn3 -> turn2, turn2 -> turn1
    // Cycle is resolved by reordering at d3: turn1 -> turn3, turn1 -> turn2
    // This creates another cycle at d2 which is resolved by
    // reordering at d2 turn1 -> turn2
    
    println("--------")
    
    engine.createFrame(turn1, d1)
    
    assert(frameTurns(d1) == Queue(turn3, turn1))
    assert(frameTurns(d2) == Queue(turn2, turn1))
    assert(frameTurns(d3) == Queue(turn2, turn3, turn1))
  }
  

}