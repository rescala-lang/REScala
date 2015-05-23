package rescala.pipelining

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import PipelineBuffer.pipelineFor
import rescala.graph.Committable
import rescala.graph.Reactive
import rescala.graph.WriteFrame
import rescala.propagation.LevelQueue
import rescala.propagation.TurnImpl
import rescala.turns.Turn
import rescala.util.JavaFunctionsImplicits.buildUnaryOp
import rescala.graph.WriteFrame
import rescala.propagation.QueueAction
import rescala.propagation.RequeueReactive
import rescala.propagation.EnqueueDependencies
import rescala.propagation.PropagateNoChanges
import rescala.propagation.DoNothing

object PipeliningTurn {

  private object lockPhaseLock

  // For testing
  protected[rescala] var numSuspiciousNotEvaluatedFrames = 0

}

class PipeliningTurn(override val engine: PipelineEngine, randomizeDeps: Boolean = false) 
  extends TurnImpl 
  with PropagateNoChanges 
  with SequentialFrameCreator {

  import PipelineBuffer._

  /**
   * Remember all reactives for which a frame was created during this turn
   */
  protected[pipelining] var framedReactives = new AtomicReference[Set[Reactive]](Set())

  val thread = Thread.currentThread()

  private object createFramesLock

  private def markReactiveFramed(reactive: Reactive) = {
    import rescala.util.JavaFunctionsImplicits._
    framedReactives.getAndUpdate { reactives: Set[Reactive] => reactives + reactive }
  }

  override implicit def currentTurn: PipeliningTurn = this

  override def create[T <: Reactive](dependencies: Set[Reactive], dynamic: Boolean)(f: => T): T = {
    val reactive = f
    engine.createFrame(this, reactive)
    markReactiveFramed(reactive)
    ensureLevel(reactive, dependencies)
    if (dynamic) {
      evaluate(reactive)
    } else dependencies.foreach(register(reactive))
    reactive
  }

  private def needsReevaluation(reactive: Reactive) = {
    val incoming = reactive.incoming.get
    if (incoming.isEmpty)
      true
    else reactive.incoming.get.exists {
      incomingDep =>
        pipelineFor(incomingDep).findFrame {
          _ match {
            case Some(frame) => frame.isTouched
            case None        => false
          }
        }
    }
  }

  override def evaluate(head: Reactive) = {
    assert(pipelineFor(head).hasFrame(this), "No frame was created in turn " + this + " for " + head)

    // TODO Modify queue for nochanges
    // Marks all nodes written, which cannot be reached anymore and which was pruned
    // in order not to block pipelining longer on these nodes
    //  markUnreachablePrunedNodesWritten(head)

    pipelineFor(head).waitUntilCanWrite(this)

    println(s"${Thread.currentThread().getId} EVALUATE $head during $this")

    var frameFound = false;
    head.pipeline.foreachFrameTopDown { frame =>
      if (frame.turn == this) {
        frameFound = true
      } else if (!frameFound) {
        assert(this > frame.turn)
      } else {
        assert(frame.turn > this)
      }
    }

    val writeFrame = pipelineFor(head).findFrame {
      _ match {
        case Some(frame @ WriteFrame(_, _)) => frame
        case _                              => throw new AssertionError("No correct write frame")
      }
    }

    // Check whether this frame is suspicious for not the evaluate
    val evaluateFrame = needsReevaluation(head)

    if (evaluateFrame) {
      pipelineFor(head).markTouched
      val queueAction = super.evaluate(head)
      queueAction match {
        case EnqueueDependencies =>
          commitFor(head)
          pipelineFor(head).markWritten
        case RequeueReactive =>
        // This reactive will be evaluated once more, so we cannot finish it
        case DoNothing       =>
      }
      queueAction
    } else {
      EnqueueDependencies
    }
  }

  override def evaluateNoChange(head: Reactive): QueueAction = {
    println(s"NOT EVALUATE $head")
    if (!pipelineFor(head).needFrame().isWritten) {
    commitFor(head)
    pipelineFor(head).markWritten
    }
    requeue(head, changed = false, level = -1, action = EnqueueDependencies)
    EnqueueDependencies
  }

  private def commitFor(head: Reactive): Unit = {
    val buffersToCommit = pipelineFor(head).createdBuffers.asInstanceOf[Set[Committable]]

    buffersToCommit.foreach { _.commit }
    toCommit --= buffersToCommit

  }

  private def hasWriteableFrame(buffer: PipelineBuffer): Boolean = {
    buffer.findFrame {
      _ match {
        case None        => false
        case Some(frame) => !frame.isWritten
      }
    }
  }

  override def register(sink: Reactive)(source: Reactive): Unit = createFramesLock.synchronized {
    val needToAddDep = !source.outgoing.get.contains(sink)
    val sourcePipeline = pipelineFor(source)
    val sourceHasFrame = hasWriteableFrame(sourcePipeline)
    if (needToAddDep && sourceHasFrame) {
      // Dont need to create frames, because a frame is already there
      source.outgoing.transform { _ + sink }
    } else if (!sourceHasFrame && needToAddDep) {
      println(s"Create dynamic frome at $source for $sink")
      // Create a dynamic read frame
      val readFrame = engine.createDynamicReadFrameFrame(this, from = sink, at = source)
      markReactiveFramed(source)
      // This writes in the dynamic read frame, but only in outgoings
      source.outgoing.transform { _ + sink }

      // this defect in frames after this frames we need to repair
      // Get all write frames after the read frame -> need to propagate the new dependency to them
      val framesAfterDynamicRead: List[WriteFrame[_]] = pipelineFor(source).writeFramesAfter(readFrame)
      // TODO need to sync probalbly on that?
      framesAfterDynamicRead.foreach { frame =>
        frame.turn.asInstanceOf[PipeliningTurn].createFramesLock.synchronized {
          source.outgoing.transform(_ + sink)(frame.turn)
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

      commitFor(source)

      readFrame.markWritten()
    }
  }

  override def unregister(sink: Reactive)(source: Reactive): Unit = createFramesLock.synchronized {
    //  val dropFrame = source.createDynamicDropFrame(sink)
    //  sink.registerDynamicFrame(dropFrame)

    println(s"Unregister $source as incoming of $sink")

    val sourcePipeline = pipelineFor(source)
    val sourceHasFrame = hasWriteableFrame(sourcePipeline)
    val needToRemoveDep = source.outgoing.get.contains(sink)

    if (needToRemoveDep) {
      val dropFrame = if (!sourceHasFrame) {
        println("Need to create drop frame")
        markReactiveFramed(source)
        pipelineFor(source).createDynamicDropFrame(sink)
      } else {
        val writeFrame = pipelineFor(source).findFrame { _.get } // Can use this frame if it is marked written? Should not make a difference
        // But then it need to be written already because it must evaluated first
        // assert(writeFrame.isWritten, "Frame of an incoming dependency needs to be evaluated first")
        writeFrame
      }

      assert(!dropFrame.isWritten)

      dropFrame.markTouched()

      println(s"Remove sink $sink from source $source")
      println(sourcePipeline.getPipelineFrames())
      source.outgoing.transform { _ - sink }
      val writeFramesAfterDrop = pipelineFor(source).writeFramesAfter(dropFrame)
      writeFramesAfterDrop.foreach { frame => source.outgoing.transform(_ - sink)(frame.turn) }

      // Cannot simply remove sink from the queue of the turns because sink may be reachable
      // though another valid path, nevertheless, the turns could wait for this frame already

      val turnsAfterDynamicRead = writeFramesAfterDrop.map(_.turn.asInstanceOf[PipeliningTurn])
      println(s"Turn after dynamic drop $turnsAfterDynamicRead")

      turnsAfterDynamicRead.foreach { turn =>
        pipelineFor(sink).findFrame({
          _ match {
            case None => // No frame for this turn at sink, lucky case, dont do anything
            case Some(frame) =>
              println(s"Frame for $turn at $sink marked suspicious")
              frame.markSuspicious() // this frame may not need to be evaluated, but this can not be decided before the frame should be evaluated
          }
        })(turn)
      }

      dropFrame match {
        case WriteFrame(_, _) =>
        case _ =>
          commitFor(source)
          dropFrame.markWritten()
      }

    }

  }

  // lock phases cannot run in parrallel currently,......
  override def lockPhase(initialWrites: List[Reactive]): Unit = createFramesLock.synchronized {
    import rescala.util.JavaFunctionsImplicits._
    val newFramedReactives = createFrames(initialWrites)
    // Now there may already be some additional frames, so cannot remove them
    framedReactives.getAndUpdate { reactives: Set[Reactive] => reactives ++ newFramedReactives }
  }

  override def commitPhase(): Unit = {
    // TODO should not be needed anymore because of pruning. But pruning does not handle dynamic dependencies by now
    println(s"Left to commit $toCommit")
    super.commitPhase()
    framedReactives.get.foreach(r => {
      if (!pipelineFor(r).needFrame().isWritten)
        pipelineFor(r).markWritten
    })

  }

  override def releasePhase(): Unit = {
    engine.turnCompleted(this)
  }

  override def toString = {
    s"PipeliningTurn(${super.toString})"
  }

  override def >(other: Turn) = {
    other == this || engine.waitsOn(this, other.asInstanceOf[PipeliningTurn])
  }

}