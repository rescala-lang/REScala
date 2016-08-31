package rescala.pipelining.tests

import org.scalatest.FlatSpec
import rescala.pipelining.util.LogUtils
import rescala.pipelining.{Pipeline, PipelineEngine, PipeliningTurn}
import rescala.reactives.{Signals, Var}

class PrunedFramesTest extends FlatSpec {

  it should "test Frames Of Pruned Nodes Are Marked" in {

    implicit val engine = new PipelineEngine()

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

    checkMark = true
    LogUtils.log("======")
    source.set(1)
  }

}
