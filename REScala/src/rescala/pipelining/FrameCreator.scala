package rescala.pipelining

import rescala.graph.Reactive
import rescala.propagation.LevelQueue
import rescala.turns.Turn

trait FrameCreator {

  protected[this] def createFrames(initialReactives: List[Reactive]): Unit

}

trait QueueBasedFrameCreator extends FrameCreator {

  self: PipeliningTurn =>

  val engine: PipelineEngine

  protected[this] final def evaluateQueue(initialWrites: List[Reactive])(op: Reactive => Unit) = {
    val lq = new LevelQueue()(this)
    initialWrites.foreach(lq.enqueue(-1))

    var seen = Set[Reactive]()
    // Create frames for all reachable reactives
    lq.evaluateQueue { reactive =>
      if (!seen.contains(reactive)) {
        seen += reactive
        Pipeline(reactive).dynamicLock.lock()
        op(reactive)
        val outgoings = reactive.outgoing.get(this)
        outgoings.foreach { lq.enqueue(-1) }
        Pipeline(reactive).dynamicLock.unlock()
      }
    }
  }

  protected[this] final def createFrame(reactive: Reactive): Unit = {
    // Need to check whether a frame is already there because a frame by a dynamic dependency
    // discovery might have been created
    val pipeline = Pipeline(reactive)
    if (!pipeline.hasFrame)
      pipeline.createFrame
  }

  protected[this] override def createFrames(initialWrites: List[Reactive]) = {
    evaluateQueue(initialWrites) { reactive =>
      markReactiveFramed(reactive, reactive => createFrame(reactive))
    }
  }

}