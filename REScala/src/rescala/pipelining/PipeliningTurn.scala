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

class PipeliningTurn(override val engine: PipelineEngine, randomizeDeps: Boolean = false) extends TurnImpl {

  /**
   * Remember all reactives for which a frame was created during this turn
   */
  protected[pipelining] var framedReactives: Set[Reactive] = Set()

  override def waitsOnFrame(other: Turn) = other == this || engine.waitsOn(this, other.asInstanceOf[PipeliningTurn])

  override def waitsOnLock[T](op: => T): T = engine.graphLocked(op)

  val allNodesQueue = new LevelQueue

  protected override def requeue(head: Reactive, changed: Boolean, level: Int, redo: Boolean): Unit = {
    if (redo)
      allNodesQueue.enqueue(level, changed)(head)
    else head.outgoing.get.foreach(allNodesQueue.enqueue(level, changed))
    super.requeue(head, changed, level, redo)
  }
  
  private def markUnreachablePrunedNodesWritten(head : Reactive) = {
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

  override def evaluate(head: Reactive) = {
    assert(head.hasFrame(this), "No frame was created in turn " + this + " for " + head)

    // Marks all nodes written, which cannot be reached anymore and which was pruned
    // in order not to block pipelining longer on these nodes
    markUnreachablePrunedNodesWritten(head)

    head.waitUntilCanWrite
    head.markTouched

    super.evaluate(head)

    head.markWritten
  }

  // lock phases cannot run in parrallel currently,......
  override def lockPhase(initialWrites: List[Reactive]): Unit = {
    engine.synchronized {
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

}