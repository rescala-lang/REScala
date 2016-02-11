package tests.rescala.pipelining

import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.mock.MockitoSugar
import rescala.Var
import rescala.Signals
import rescala.pipelining.PipelineEngine
import rescala.turns.Turn
import org.junit.Test
import rescala.pipelining.Pipeline
import rescala.pipelining.PipeliningTurn

class PrunedFramesTest extends AssertionsForJUnit with MockitoSugar {
  
  implicit val engine = PipelineEngine
  
  var checkMark = false
  
  val source = Var(0)
  val dep1Level1 = Signals.static(source)(implicit t => source.get)
  val dep2Level1 = Signals.static(source)(implicit t => 0)
  val dep1Level2 = Signals.static(dep1Level1)(implicit t => dep1Level1.get)
  val dep2Level2Pruned = Signals.static(dep2Level1)(implicit t => {
    assert(!checkMark, "Pruned node is evaluated")
    dep2Level1.get
  })
  val dep1Level3 = Signals.static(dep1Level2)(implicit t => {
    assert(!checkMark || Pipeline.pipelineFor(dep2Level2Pruned).needFrame()(t.asInstanceOf[PipeliningTurn]).isWritten)
    dep1Level2.get
  })
  
  @Test
  def testFramesOfPrunedNodesAreMarked() = {
    checkMark = true
    println("======")
    source.set(1)
  }

}