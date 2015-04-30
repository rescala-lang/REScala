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
import rescala.graph.Reactive
import java.util.Random
import java.util.concurrent.CyclicBarrier
import rescala.pipelining.PipeliningTurn
import scala.annotation.tailrec

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
  val d1 = Signals.static(s1, s2) { implicit t =>
    s1.get - s2.get
  }
  val d2 = Signals.static(s1, s2) { implicit t =>
    s1.get - 2 * s2.get
  }

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
  def testMultipleUpdates() = {
    val turns = List.fill(6)(engine.makeTurn)
    val sources = List(s2, s1, s1, s2, s2, s1)

    def makeFramesForUpdate(turn: PipeliningTurn, source: Reactive) = {
      engine.createFrame(turn, source)
      engine.createFrame(turn, d1)
      engine.createFrame(turn, d2)
    }

    turns.zip(sources).foreach({
      case (turn, source) =>
        turn.lockPhase(List(source))
    })

    assert(d1.getPipelineFrames().map(_.turn) == Queue() ++ turns)
    assert(d2.getPipelineFrames().map(_.turn) == Queue() ++ turns)

    val x = 1;
  }

  @Test
  def testEvaluationSequential() = {
    // Everything sequential => there cannot be any conflict
    assert(d1.now == 0)
    assert(d2.now == 0)
    s1.set(10)
    assert(d1.now == 10)
    assert(d2.now == 10)
    s2.set(5)
    assert(d1.now == 5)
    assert(d2.now == 0)
  }

}