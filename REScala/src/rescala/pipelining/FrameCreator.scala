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

  protected[this] final def evaluateQueue(initialWrites: List[Reactive]) = {
    val lq = new LevelQueue()(this)
    initialWrites.foreach(lq.enqueue(-1))

    var seen = Set[Reactive]()
    // Create frames for all reachable reactives
    lq.evaluateQueue { reactive =>
      if (!seen.contains(reactive)) {
        seen += reactive
        val pipeline = Pipeline(reactive)
        pipeline.dynamicLock.lock()
        markReactiveFramed(reactive, reactive => {
          if (!pipeline.hasFrame)
            createFrame(pipeline)
        })
        val outgoings = reactive.outgoing.get(this)
        outgoings.foreach { lq.enqueue(-1) }
        pipeline.dynamicLock.unlock()
      }
    }
  }
  
  protected[this] def createFrame(pipeline : Pipeline) = pipeline.createFrame


  protected[this] override def createFrames(initialWrites: List[Reactive]) = {
    evaluateQueue(initialWrites)
  }

}