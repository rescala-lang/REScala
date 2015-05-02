package rescala.pipelining

import rescala.graph.Reactive
import rescala.turns.Turn
import rescala.propagation.TurnImpl
import rescala.graph.Committable
import rescala.propagation.LevelQueue
import rescala.graph.ReactiveFrame
import rescala.graph.ReactiveFrame
import rescala.Signal
import rescala.graph.Pulsing
import rescala.graph.ReevaluationResult._
import rescala.graph.{ DynamicReadFrame, DynamicDropFrame, WriteFrame }
import rescala.graph.DynamicReadFrame
import rescala.graph.Frame
import rescala.graph.ReactiveFrame
import rescala.graph.Framed
import rescala.graph.DynamicReevaluation

object PipeliningTurn {

  private object lockPhaseLock
}

class PipeliningTurn(override val engine: PipelineEngine, randomizeDeps: Boolean = false) extends TurnImpl {

  /**
   * Remember all reactives for which a frame was created during this turn
   */
  protected[pipelining] var framedReactives: Set[Reactive] = Set()
  protected[pipelining] var preceedingTurns: Set[PipeliningTurn] = Set()
  protected[pipelining] var causedReactives: Map[PipeliningTurn, Set[Reactive]] = Map()

  override def waitsOnLock[T](op: => T): T = engine.graphLocked(op)

  val allNodesQueue = new LevelQueue

  protected override def requeue(head: Reactive, changed: Boolean, level: Int, redo: Boolean): Unit = {
    if (redo)
      allNodesQueue.enqueue(level, changed)(head)
    else head.outgoing.get.foreach(allNodesQueue.enqueue(level, changed))
    super.requeue(head, changed, level, redo)
  }

  private def markUnreachablePrunedNodesWritten(head: Reactive) = {
    // In all queue are all nodes, not just the changed ones, for 
    allNodesQueue.processQueueUntil { allHead =>
      assert(allHead.hasFrame)
      if (allHead.level.get < head.level.get) {
        allHead.markWritten
        false
      } else {
        true
      }
    }
  }

  override def create[T <: Reactive](dependencies: Set[Reactive], dynamic: Boolean)(f: => T): T = {
    val reactive = f
    ensureLevel(reactive, dependencies)
    if (dynamic) {
      engine.createFrame(this, reactive)
      framedReactives += reactive
      evaluate(reactive)
    } else dependencies.foreach(register(reactive))
    reactive
  }

  override def evaluate(head: Reactive) = {
    assert(head.hasFrame(this), "No frame was created in turn " + this + " for " + head)

    // Marks all nodes written, which cannot be reached anymore and which was pruned
    // in order not to block pipelining longer on these nodes
    markUnreachablePrunedNodesWritten(head)

    head.waitUntilCanWrite
    // head.fillFrame
    head.markTouched

    assert(preceedingTurns.forall(turn =>
      head.findFrame {
        _ match {
          case None        => true
          case Some(frame) => frame.isWritten
        }
      }(turn)))

    super.evaluate(head)

    head.markWritten
  }

  override def register(sink: Reactive)(source: Reactive): Unit = {
    if (!source.hasFrame && !sink.outgoing.get.contains(source)) {
      println(s"Create dynamic frome at $source for $sink")
      // Create a dynamic read frame
      val readFrame = engine.createDynamicReadFrameFrame(this, from = sink, at = source)
      // This writes in the dynamic read frame, but only in outgoings
      source.fillFrame
      source.outgoing.transform { _ + sink }
      readFrame.markWritten()

      // this defect in frames after this frames we need to repair
      // Get all write frames after the read frame -> need to propagate the new dependency to them
      val framesAfterDynamicRead: List[WriteFrame[_ <: ReactiveFrame]] = source.writeFramesAfter(readFrame)
      // TODO need to sync probalbly on that?
      framesAfterDynamicRead.foreach { frame =>
        frame.content.outgoing.transform(_ + sink)(frame.turn)
      }

      // Need to create frames for the turns after the dynamic read at the new nodes
      val turnsAfterDynamicRead = framesAfterDynamicRead.map(_.turn.asInstanceOf[PipeliningTurn])
      println(s"Turns after dynamic read $turnsAfterDynamicRead")

      if (turnsAfterDynamicRead.nonEmpty) {
        // Queue based create frames at reachable reactives
        val queue = new LevelQueue
        queue.enqueue(-1)(source)
        queue.evaluateQueue { reactive =>
          var anyFrameCreated = false
          for (turn <- turnsAfterDynamicRead) {
            val frameCreated = engine.createFrameAfter(this, turn, reactive)
            anyFrameCreated ||= frameCreated
          }
          // Only need to continue created frames, if one was created
          if (anyFrameCreated)
            reactive.outgoing.get.foreach { queue.enqueue(-1) }
        }

      }

      // Add the new nodes in the queue of the turns
      turnsAfterDynamicRead.foreach { turn =>
        turn.admit(sink)
      }

      framedReactives += source
    }
  }

  override def unregister(sink: Reactive)(source: Reactive): Unit = {
    //  val dropFrame = source.createDynamicDropFrame(sink)
    //  sink.registerDynamicFrame(dropFrame)
    super.unregister(sink)(source)
  }

  // lock phases cannot run in parrallel currently,......
  override def lockPhase(initialWrites: List[Reactive]): Unit = {
    PipeliningTurn.lockPhaseLock.synchronized {
      def createFrame(reactive: Reactive): Unit = {
        engine.createFrame(this, reactive)
        assert(reactive.hasFrame(this))
        framedReactives += reactive
      }

      val lq = new LevelQueue()
      initialWrites.foreach { createFrame(_) }
      initialWrites.foreach(lq.enqueue(-1))

      // Create frames for all reachable reactives
      lq.evaluateQueue { reactive =>
        val outgoings = reactive.outgoing.get
        outgoings.foreach(createFrame(_))
        outgoings.foreach { lq.enqueue(-1) }
      }
    }
  }

  override def releasePhase(): Unit = {
    framedReactives.foreach(_.markWritten)
    engine.turnCompleted(this)
  }

  override def toString = {
    s"PipeliningTurn(${super.toString})"
  }

  override def >~(other: Turn) = {
    preceedingTurns.contains(other.asInstanceOf[PipeliningTurn])
  }

  override def >>~(other: Turn) = {
    other == this || engine.waitsOn(this, other.asInstanceOf[PipeliningTurn])
  }

}