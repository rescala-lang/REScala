package rescala.pipelining

import rescala.graph.Reactive
import rescala.propagation.LevelQueue
import rescala.turns.Turn

trait FrameCreator {

  protected[this] def createFrames(initialReactives: List[Reactive]): Set[Reactive]

}

trait QueueBasedFrameCreator extends FrameCreator {

  self: PipeliningTurn =>

  val engine: PipelineEngine

  protected[this] final def evaluateQueue(initialWrites: List[Reactive])(op: Reactive => Unit) = {
    val lq = new LevelQueue()(this)
    initialWrites.foreach(lq.enqueue(-1))

    // Create frames for all reachable reactives
    lq.evaluateQueue { reactive =>
      op(reactive)
      val outgoings = reactive.outgoing.get(this)
      outgoings.foreach { lq.enqueue(-1) }
    }
  }

  protected[this] final def createFrame(reactive: Reactive): Unit = {
    engine.createFrame(this, reactive)
  }

  protected[this] override def createFrames(initialWrites: List[Reactive]) = {
    var framedReactives = Set[Reactive]()

    evaluateQueue(initialWrites) { reactive =>
      createFrame(reactive)
      framedReactives += reactive
    }

    framedReactives
  }

}