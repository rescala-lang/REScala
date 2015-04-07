package rescala.pipelining.tests

import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.Var
import rescala.pipelining.PipelineEngine
import rescala.Signals
import org.junit.Test
import rescala.pipelining.tests.PipelineTestUtils._
import scala.collection.immutable.Queue

class TransitiveResolvingTest extends AssertionsForJUnit with MockitoSugar {
  
  implicit val engine = new PipelineEngine
  
  /*
   * This tests uses the following topology which allows three
   * parallel updated with a transitive waiting relation
   * 
   * S1    S2   S3
   * | \  / \  / |
   * |  \/   \/  |
   * |  /\   /\  |
   * | /  \ /  \ |
   * D1   D2    D3
   */
  
  val s1 = Var(0)
  val s2 = Var(0)
  val s3 = Var(0)
  val d1 = Signals.static(s1,s2){implicit t => s1.get - s2.get}
  val d2 = Signals.static(s1,s3){implicit t => s1.get - s3.get}
  val d3 = Signals.static(s2,s3){implicit t => s2.get - s3.get}
  
  @Test
  def resolveTransitiveConflict() = {
    val turn1 = engine.makeTurn
    val turn2 = engine.makeTurn
    val turn3 = engine.makeTurn
    
    engine.createFrame(turn1, d1)
    engine.createFrame(turn3, d2)
    engine.createFrame(turn2, d3)
    engine.createFrame(turn2, d1)
    engine.createFrame(turn3, d3)
    
    assert(frameTurns(d1) == Queue(turn1, turn2))
    assert(frameTurns(d2) == Queue(turn3))
    assert(frameTurns(d3) == Queue(turn2, turn3))
    
    // No put turn1 on d2, this creates a cycle:
    // At d1: turn1 -> turn2
    // At d2: turn2 -> turn3 and then
    // At d3: turn3 -> turn1
    // Cycle is resolved by reordering at d2: turn2 -> turn1
    
    println("--------")
    
    engine.createFrame(turn1, d2)
    
    assert(frameTurns(d1) == Queue(turn2, turn1))
    assert(frameTurns(d2) == Queue(turn3, turn1))
    assert(frameTurns(d3) == Queue(turn2, turn3))
  }
  
  @Test
  def resolveTransistiveConflictMultipleTurns() = {
    val turn1 = engine.makeTurn // From S1
    val turn2 = engine.makeTurn // From S2
    val turn3 = engine.makeTurn // From S3
    val turn4 = engine.makeTurn // From S2
    val turn5 = engine.makeTurn // From S3
    
    println("Turn1: " + turn1)
    println("Turn2: " + turn2)
    println("Turn3: " + turn3)
    println("Turn4: " + turn4)
    println("Turn5: " + turn5)
    
    engine.createFrame(turn5, d2)
    engine.createFrame(turn2, d3)
    engine.createFrame(turn3, d3)
    engine.createFrame(turn4, d3)
    engine.createFrame(turn5, d3)
    
    assert(frameTurns(d1) == Queue())
    assert(frameTurns(d2) == Queue(turn5))
    assert(frameTurns(d3) == Queue(turn2, turn3, turn4, turn5))
    
    assert(engine.getOrdering == Map(
        ((turn2, turn3) -> Set(d3)),
        ((turn2, turn4) -> Set(d3)),
        ((turn2, turn5) -> Set(d3)),
        ((turn3, turn4) -> Set(d3)),
        ((turn3, turn5) -> Set(d3)),
        ((turn4, turn5) -> Set(d3))
      ))
    assert(engine.getWaitingEdges == Map(
        (turn3 -> Set(turn2)),
        (turn4 -> Set(turn2, turn3)),
        (turn5 -> Set(turn2, turn3, turn4))
      ))
    
    // First cycle: now  at d3: turn5 -> turn3
    // put turn 3 on d2, then: turn3 -> turn5
    // Cycle is resolved by moving turn3 back in d3
    
    engine.createFrame(turn3, d2)
    assert(frameTurns(d1) == Queue())
    assert(frameTurns(d2) == Queue(turn5, turn3))
    assert(frameTurns(d3) == Queue(turn2, turn4, turn5, turn3))
    
    assert(engine.getOrdering == Map(
        ((turn2, turn3) -> Set(d3)),
        ((turn2, turn4) -> Set(d3)),
        ((turn2, turn5) -> Set(d3)),
        ((turn4, turn5) -> Set(d3)),
        ((turn4, turn3) -> Set(d3)),
        ((turn5, turn3) -> Set(d3, d2))
      ))
    assert(engine.getWaitingEdges == Map(
        (turn3 -> Set(turn2, turn4, turn5)),
        (turn4 -> Set(turn2)),
        (turn5 -> Set(turn2, turn4))
      ))
    
    
    engine.createFrame(turn1, d1)
    
    assert(frameTurns(d1) == Queue(turn1))
    assert(frameTurns(d2) == Queue(turn5, turn3))
    assert(frameTurns(d3) == Queue(turn2, turn4, turn5, turn3))
    
    engine.createFrame(turn4, d1)
    
    assert(frameTurns(d1) == Queue(turn1, turn4))
    assert(frameTurns(d2) == Queue(turn5, turn3))
    assert(frameTurns(d3) == Queue(turn2, turn4, turn5, turn3))
    
    
    
    // Second cycle: now at d3: turn4 -> turn2
    // put turn 2 on d1, then:  turn2 -> turn4
    // Cycle is resolved by moving turn2 back in d3
    engine.createFrame(turn2, d1)
    
    assert(frameTurns(d1) == Queue(turn1, turn4, turn2))
    assert(frameTurns(d2) == Queue(turn5, turn3))
    assert(frameTurns(d3) == Queue(turn4, turn2, turn5, turn3))
    
    // Third cycle: now at d1: turn1 -> turn4, turn2
    // now at d3: turn2, turn4 -> turn5, turn3
    // put turn1 on d2, then: turn1 -> turn3, turn 5
    // Cycle is resolved by moving turn1 back at d1
    engine.createFrame(turn1, d2)
    assert(frameTurns(d1) == Queue(turn4, turn2, turn1))
    assert(frameTurns(d2) == Queue(turn5, turn3, turn1))
    assert(frameTurns(d3) == Queue(turn4, turn2, turn5, turn3))
    
    assert(engine.getOrdering == Map(
        ((turn2, turn1) -> Set(d1)),
        ((turn2, turn3) -> Set(d3)),
        ((turn2, turn5) -> Set(d3)),
        ((turn3, turn1) -> Set(d2)),
        ((turn4, turn5) -> Set(d3)),
        ((turn4, turn3) -> Set(d3)),
        ((turn4, turn2) -> Set(d1, d3)),
        ((turn4, turn1) -> Set(d1)),
        ((turn5, turn3) -> Set(d3, d2)),
        ((turn5, turn1) -> Set(d2))
      ))
    assert(engine.getWaitingEdges == Map(
        (turn3 -> Set(turn2, turn4, turn5)),
        (turn5 -> Set(turn2, turn4)),
        (turn2 -> Set(turn4)),
        (turn1 -> Set(turn4, turn2, turn5, turn3))
      ))
    
    
 /*   assert(frameTurns(d1) == Queue(turn1, turn2))
    assert(frameTurns(d2) == Queue(turn3))
    assert(frameTurns(d3) == Queue(turn2, turn3))
    
    // No put turn1 on d2, this creates a cycle:
    // At d1: turn1 -> turn2
    // At d2: turn2 -> turn3 and then
    // At d3: turn3 -> turn1
    // Cycle is resolved by reordering at d2: turn2 -> turn1
    
    engine.createFrame(turn1, d2)
    
    assert(frameTurns(d1) == Queue(turn2, turn1))
    assert(frameTurns(d2) == Queue(turn3, turn1))
    assert(frameTurns(d3) == Queue(turn2, turn3))*/
  }

}