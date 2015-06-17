package rescala.pipelining

import rescala.graph.Reactive
import rescala.propagation.LevelQueue
import rescala.turns.Turn
import java.util.concurrent.locks.ReentrantLock

trait FrameCreator {

  protected[this] def createFrames(initialReactives: List[Reactive]): Unit

}

trait QueueBasedFrameCreator extends FrameCreator {

  self: PipeliningTurn =>

  val engine: PipelineEngine

  private val frameCompletedLock = new ReentrantLock
  frameCompletedLock.lock()

  def waitUntilFramingCompleted() = {
    frameCompletedLock.lock()
    frameCompletedLock.unlock()
  }

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
        if (!pipeline.hasFrame) {
          markReactiveFramed(reactive, _ => createFrame(pipeline))
          val outgoings = reactive.outgoing.get(this)
          outgoings.foreach { lq.enqueue(-1) }
        }
        pipeline.dynamicLock.unlock()
      }
    }
  }

  protected[this] def createFrame(pipeline: Pipeline): Unit = pipeline.createFrame

  protected[this] override def createFrames(initialWrites: List[Reactive]) = {
    evaluateQueue(initialWrites)
    frameCompletedLock.unlock()
  }

}