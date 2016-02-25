package rescala.pipelining

import rescala.graph.Reactive
import rescala.pipelining.util.LogUtils
import LogUtils._
import rescala.pipelining.propagation.PipelineQueue

private[pipelining] trait FrameCreator {

  protected[this] def createFrames(initialReactives: List[Reactive[PipelineStruct.type]]): Unit

}

private[pipelining] trait QueueBasedFrameCreator extends FrameCreator {

  self: PipeliningTurn =>

  type S = PipelineStruct.type


  protected[this] final def evaluateQueue(initialWrites: List[Reactive[S]]) = {
    val lq = new PipelineQueue()(this)
    initialWrites.foreach(lq.enqueue(-1))

    var seen = Set[Reactive[S]]()
    // Create frames for all reachable reactives
    lq.evaluateQueue { reactive =>
      if (!seen.contains(reactive)) {
        seen += reactive
        val pipeline = Pipeline(reactive)
        pipeline.lockDynamic {
        markReactiveFramed(pipeline, _ => {
          if (!pipeline.hasFrame) {
            log(s"Create frame for $this at $reactive")
            createFrame(pipeline)
          }
        })
        val outgoings = reactive.bud.outgoing(this)
        outgoings.foreach { lq.enqueue(-1) }
        }
      }
    }
  }

  protected[this] def createFrame(pipeline : Pipeline) = pipeline.createFrame


  protected[this] override def createFrames(initialWrites: List[Reactive[S]]) = {
    evaluateQueue(initialWrites)
  }

}