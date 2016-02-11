package rescala.pipelining

import rescala.graph.Reactive
import rescala.pipelining.LogUtils._
import rescala.propagation.LevelQueue

trait FrameCreator {

  protected[this] def createFrames(initialReactives: List[Reactive[PipelineSpores.type]]): Unit

}

trait QueueBasedFrameCreator extends FrameCreator {

  self: PipeliningTurn =>

  type S = PipelineSpores.type

  val engine: PipelineEngine

  protected[this] final def evaluateQueue(initialWrites: List[Reactive[S]]) = {
    val lq = new LevelQueue()(this)
    initialWrites.foreach(lq.enqueue(-1))

    var seen = Set[Reactive[S]]()
    // Create frames for all reachable reactives
    lq.evaluateQueue { reactive =>
      if (!seen.contains(reactive)) {
        seen += reactive
        val pipeline = Pipeline(reactive)
        pipeline.lockDynamic {
        markReactiveFramed(reactive, reactive => {
          if (!pipeline.hasFrame) {
            println(s"Create frame for $this at $reactive")
            createFrame(pipeline)
          }
        })
        val outgoings = reactive.outgoing(this)
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