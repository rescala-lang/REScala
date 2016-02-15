package rescala.pipelining.tests

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.graph.Reactive
import rescala.pipelining.Pipeline._
import rescala.pipelining.{Pipeline, PipelineEngine, PipelineSpores, PipeliningTurn}
import rescala.reactives.Signals

import scala.collection.immutable.Queue

class ConflictResolvingTest extends AssertionsForJUnit with MockitoSugar {



  implicit val engine = new PipelineEngine()
  import engine.Var

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




  @Test
  def testMultipleUpdates() = {

    val s1 = Var(0)
    val s2 = Var(0)
    val d1 = Signals.static(s1, s2) { implicit t =>
      s1.get - s2.get
    }
    val d2 = Signals.static(s1, s2) { implicit t =>
      s1.get - 2 * s2.get
    }

    val turns = List.fill(6)(engine.makeTurn)
    val sources = List(s2, s1, s1, s2, s2, s1)

    def makeFramesForUpdate(turn: PipeliningTurn, source: Reactive[PipelineSpores.type]) = {
      Pipeline(source).createFrame(turn)
      Pipeline(d1).createFrame(turn)
      Pipeline(d2).createFrame(turn)
    }

    turns.zip(sources).foreach({
      case (turn, source) =>
        turn.lockPhase(List(source))
    })

    assert(pipelineFor(d1).getPipelineFrames().map(_.turn) == Queue() ++ turns)
    assert(pipelineFor(d2).getPipelineFrames().map(_.turn) == Queue() ++ turns)

    val x = 1;

  }

  @Test
  def testEvaluationSequential() = {

    val s1 = Var(0)
    val s2 = Var(0)
    val d1 = Signals.static(s1, s2) { implicit t =>
      s1.get - s2.get
    }
    val d2 = Signals.static(s1, s2) { implicit t =>
      s1.get - 2 * s2.get
    }

    // Everything sequential => there cannot be any conflict
    assert(d1.now == 0)
    assert(d2.now == 0)
    s1.set(10)
    assert(d1.now == 10)
    assert(d2.now == 10)
    s2.set(5)
    assert(d1.now == 5)
    assert(d2.now == 0)

    assert(engine.getTurnOrder().isEmpty)

  }

}