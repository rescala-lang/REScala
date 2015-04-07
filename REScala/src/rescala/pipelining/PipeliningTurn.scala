package rescala.pipelining

import rescala.graph.Reactive
import rescala.turns.Turn
import rescala.propagation.TurnImpl
import rescala.graph.Committable
import rescala.propagation.LevelQueue
import rescala.graph.ReactiveFrame
import rescala.graph.ReactiveFrame

class PipeliningTurn(override val engine: PipelineEngine) extends TurnImpl {

  /**
   * Remember all reactives for which a frame was created during this turn
   */
  private var framedReactives : Set[Reactive] = Set()
  
  override def evaluate(head: Reactive) = {
    // TODO need to wait here, until the frame before is written
    super.evaluate(head)
    // Mark the frame as written -> the turn will not touch this frame again
    head.markWritten
  }

  override def lockPhase(initialWrites: List[Reactive]): Unit = {
    val lq = new LevelQueue()
    initialWrites.foreach(lq.enqueue(-1))

    // Create frames for all reachable reactives
    lq.evaluateQueue { reactive =>
      engine.createFrame(this, reactive)
      framedReactives += reactive }
  }
  
  override def releasePhase(): Unit = {
    // Mark all frames for removal
    framedReactives.foreach { _.tryRemoveFrame }
  }

}