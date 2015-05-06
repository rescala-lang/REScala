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
import rescala.graph.WriteFrame
import java.util.concurrent.atomic.AtomicReference

object PipeliningTurn {

  private object lockPhaseLock

  // For testing
  protected[rescala] var numSuspiciousNotEvaluatedFrames = 0

}

class PipeliningTurn(override val engine: PipelineEngine, randomizeDeps: Boolean = false) extends TurnImpl with SequentialFrameCreator {

  /**
   * Remember all reactives for which a frame was created during this turn
   */
  protected[pipelining] var framedReactives = new AtomicReference[Set[Reactive]](Set())
  protected[pipelining] var preceedingTurns = new AtomicReference[Set[PipeliningTurn]](Set())

  val thread = Thread.currentThread()

  override def waitsOnLock[T](op: => T): T = engine.graphLocked(op)

  import scala.language.implicitConversions
  implicit private def castTurns(turn: Turn): PipeliningTurn = turn.asInstanceOf[PipeliningTurn]

  val allNodesQueue = new LevelQueue

  private object createFramesLock

  private def markReactiveFramed(reactive: Reactive) = {
    import rescala.util.JavaFunctionsImplicits._
    framedReactives.getAndUpdate { reactives: Set[Reactive] => reactives + reactive }
  }

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
      markReactiveFramed(reactive)
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

    assert(preceedingTurns.get.forall(turn =>
      head.findFrame {
        _ match {
          case None        => true
          case Some(frame) => frame.isWritten
        }
      }(turn)), s"Illegal wait state for $this at $head: queue=${head.getPipelineFrames()} preceedingTurns=$preceedingTurns")

    val writeFrame = head.findFrame {
      _ match {
        case Some(frame @ WriteFrame(_, _)) => frame
        case _                              => throw new AssertionError("No correct write frame")
      }
    }

    // Check whether this frame is suspicious for not the evaluate
    val evaluateFrame = if (writeFrame.isSuspicious()) {
      //Dont evaluate this frame if all frames for exactly this turn of all incoming dependencies are not touched
      // Then there was no change but the frame was created for a dynamic dependency which has been removed again
      val needEvaluate = writeFrame.content.incoming.exists {
        incomingDep =>
          incomingDep.findFrame {
            _ match {
              case Some(frame) => frame.isTouched
              case None        => false
            }
          }
      }
      assert { PipeliningTurn.numSuspiciousNotEvaluatedFrames += 1; true }
      needEvaluate
    } else
      true

    if (evaluateFrame) {

      if (!head.incoming.isEmpty && !writeFrame.isTouched)
        // Only fill the frame with previous values, if it has not been visited  already -> bug with requeue? should not happen, or?
        head.fillFrame

      // head.fillFrame
      head.markTouched

      super.evaluate(head)
    }

    //Mark the frame finished in any case, such that the next turn can continue and does not to wait
    // until the frame is detected for pruning
    head.markWritten

  }

  override def register(sink: Reactive)(source: Reactive): Unit = {
    val needToAddDep = !source.outgoing.get.contains(sink)
    val sourceHasFrame = source.hasFrame
    if (needToAddDep && sourceHasFrame) {
      // TODO Can this case happen, it does not, or?
    } else if (!sourceHasFrame && needToAddDep) {
      println(s"Create dynamic frome at $source for $sink")
      // Create a dynamic read frame
      val readFrame = engine.createDynamicReadFrameFrame(this, from = sink, at = source)
      markReactiveFramed(source)
      // This writes in the dynamic read frame, but only in outgoings
      source.outgoing.transform { _ + sink }

      // this defect in frames after this frames we need to repair
      // Get all write frames after the read frame -> need to propagate the new dependency to them
      val framesAfterDynamicRead: List[WriteFrame[_ <: ReactiveFrame]] = source.writeFramesAfter(readFrame)
      // TODO need to sync probalbly on that?
      framesAfterDynamicRead.foreach { frame =>
        frame.turn.asInstanceOf[PipeliningTurn].createFramesLock.synchronized {
          frame.content.outgoing.transform(_ + sink)(frame.turn)
        }
      }

      // Need to create frames for the turns after the dynamic read at the new nodes
      val turnsAfterDynamicRead = framesAfterDynamicRead.map(_.turn.asInstanceOf[PipeliningTurn])
      println(s"Turns after dynamic read $turnsAfterDynamicRead")

      if (turnsAfterDynamicRead.nonEmpty) {
        // Queue based create frames at reachable reactives
        val queue = new LevelQueue
        queue.enqueue(-1)(sink)
        queue.evaluateQueue { reactive =>
          var anyFrameCreated = false
          for (turn <- turnsAfterDynamicRead) {
            // TODO Actually not sure that we need to lock here
            turn.createFramesLock.synchronized {
              val frameCreated = engine.createFrameAfter(this, turn, reactive)
              anyFrameCreated ||= frameCreated
            }
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

      readFrame.markWritten()
    }
  }

  override def unregister(sink: Reactive)(source: Reactive): Unit = {
    //  val dropFrame = source.createDynamicDropFrame(sink)
    //  sink.registerDynamicFrame(dropFrame)

    println(s"Unregister $source as incoming of $sink")

    val sourceHasFrame = source.hasFrame
    val needToRemoveDep = source.outgoing.get.contains(sink)

    if (needToRemoveDep) {
      val dropFrame = if (!sourceHasFrame) {
        println("Need to create drop frame")
        markReactiveFramed(source)
        source.createDynamicDropFrame(sink)
      } else {
        val writeFrame = source.findFrame { _.get } // Can use this frame if it is marked written? Should not make a difference
        // But then it need to be written already because it must evaluated first
        assert(writeFrame.isWritten, "Frame of an incoming dependency needs to be evaluated first")
        writeFrame
      }

      dropFrame.markTouched()

      println(s"Remove sink from $source")
      source.outgoing.transform { _ - sink }
      val writeFramesAfterDrop = source.writeFramesAfter(dropFrame)
      writeFramesAfterDrop.foreach { frame => frame.content.outgoing.transform(_ - sink)(frame.turn) }

      // Cannot simply remove sink from the queue of the turns because sink may be reachable
      // though another valid path, nevertheless, the turns could wait for this frame already

      val turnsAfterDynamicRead = writeFramesAfterDrop.map(_.turn.asInstanceOf[PipeliningTurn])
      println(s"Turn after dynamic drop $turnsAfterDynamicRead")

      turnsAfterDynamicRead.foreach { turn =>
        sink.findFrame({
          _ match {
            case None => // No frame for this turn at sink, lucky case, dont do anything
            case Some(frame) =>
              println(s"Frame for $turn at $sink marked suspicious")
              frame.markSuspicious() // this frame may not need to be evaluated, but this can not be decided before the frame should be evaluated
          }
        })(turn)
      }

      dropFrame.markWritten()

    }

  }

  // lock phases cannot run in parrallel currently,......
  override def lockPhase(initialWrites: List[Reactive]): Unit = createFramesLock.synchronized {
    import rescala.util.JavaFunctionsImplicits._
    val newFramedReactives = createFrames(initialWrites)
    // Now there may already be some additional frames, so cannot remove them
    framedReactives.getAndUpdate { reactives: Set[Reactive] => reactives ++ newFramedReactives }
  }

  override def releasePhase(): Unit = {
    // TODO should not be needed anymore because of pruning. But pruning does not handle dynamic dependencies by now
    framedReactives.get.foreach(_.markWritten)
    engine.turnCompleted(this)
  }

  override def toString = {
    s"PipeliningTurn(${super.toString})"
  }

  override def >~(other: Turn) = {
    preceedingTurns.get.contains(other.asInstanceOf[PipeliningTurn])
  }

  override def >>~(other: Turn) = {
    other == this || engine.waitsOn(this, other.asInstanceOf[PipeliningTurn])
  }

}