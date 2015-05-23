package rescala.pipelining.tests

import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.Var
import rescala.Signals
import rescala.pipelining.PipelineEngine
import rescala.turns.Turn
import org.junit.Test
import rescala.pipelining.PipelineBuffer
import rescala.pipelining.PipeliningTurn

class PrunedFramesTest extends AssertionsForJUnit with MockitoSugar {
  
  implicit val engine = new PipelineEngine
  
  var checkMark = false
  
  val source = Var(0)
  val dep1Level1 = Signals.static(source)(implicit t => source.get)
  val dep2Level1 = Signals.static(source)(implicit t => 0)
  val dep1Level2 = Signals.static(dep1Level1)(implicit t => dep1Level1.get)
  val dep2Level2Pruned = Signals.static(dep2Level1)(implicit t => dep2Level1.get)
  val dep1Level3 = Signals.static(dep1Level2)(implicit t => {
    assert(!checkMark || PipelineBuffer.pipelineFor(dep2Level2Pruned).needFrame()(t.asInstanceOf[PipeliningTurn]).isWritten)
    dep1Level2.get
  })
  
  @Test
  def testFramesOfPrunedNodesAreMarked() = {
    checkMark = true
    source.set(1)
  }

}