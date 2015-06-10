package rescala.pipelining

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import rescala.graph.Committable
import rescala.graph.Reactive
import rescala.graph.WriteFrame
import rescala.propagation.LevelQueue
import rescala.propagation.TurnImpl
import rescala.turns.Turn
import rescala.util.JavaFunctionsImplicits.buildUnaryOp
import rescala.propagation.QueueAction
import rescala.propagation.{ RequeueReactive, EnqueueDependencies, DoNothing }
import rescala.propagation.PropagateNoChanges
import rescala.util.JavaFunctionsImplicits._
import java.util.concurrent.locks.ReadWriteLock
import java.util.concurrent.locks.ReentrantReadWriteLock
import rescala.graph.Reactive

object PipeliningTurn {

  sealed abstract class PipelinedEvaluationRequest
  case class EvaluateNow(writeFrame: WriteFrame[BufferFrameContent]) extends PipelinedEvaluationRequest
  case object EvaluateLater extends PipelinedEvaluationRequest
  case object DontEvaluate extends PipelinedEvaluationRequest

}

class PipeliningTurn(override val engine: PipelineEngine, randomizeDeps: Boolean = false)
  extends TurnImpl
  with PropagateNoChanges
  with ParallelFrameCreator {

  import Pipeline._
  import PipeliningTurn._

  /**
   * Remember all reactives for which a frame was created during this turn
   */
  @volatile
  private var framedReactives = Set[Reactive]()
  private[pipelining] val framedReactivesLock = new ReentrantReadWriteLock()

  val thread = Thread.currentThread()

  private object createFramesLock

  private def doFramedWriteLocked(op: Set[Reactive] => Set[Reactive]): Unit = {
    val writeLock = framedReactivesLock.writeLock()
    writeLock.lock()
    framedReactives = op(framedReactives)
    writeLock.unlock()
  }

  protected[pipelining] def markReactiveFramed[T](reactive: Reactive, createFrame: Reactive => T): T = {
    var frame = null.asInstanceOf[T]
    doFramedWriteLocked(reactives => {
      frame = createFrame(reactive)
      reactives + reactive
    })
    frame
  }

  private def markReactiveUnframed(reactive: Reactive, removeFrame: Reactive => Unit) = {
    doFramedWriteLocked(reactives => {
      removeFrame(reactive)
      reactives - reactive
    })
  }

  private def readFramedReactive(op: Set[Reactive] => Unit): Unit = {
    val readLock = framedReactivesLock.readLock()
    readLock.lock()
    op(framedReactives)
    readLock.unlock()
  }

  override implicit def currentTurn: PipeliningTurn = this

  override def create[T <: Reactive](dependencies: Set[Reactive], dynamic: Boolean)(f: => T): T = {
    val reactive = f

    markReactiveFramed(reactive, engine.createFrame(this, _))
    ensureLevel(reactive, dependencies)
    if (dynamic) {
      evaluate(reactive)
    } else {
      dependencies.foreach(register(reactive))
      evaluateNoChange(reactive)
    }
    reactive
  }

  private def assertFrameOrder(head: Reactive): Boolean = {
    var frameFound = false;
    head.pipeline.foreachFrameTopDown { frame =>
      if (frame.turn == this) {
        frameFound = true
      } else if (!frameFound) {
        assert(this >= frame.turn)
        assert(frame.isWritten)
      } else {
        assert(frame.turn >= this)
      }
    }
    true
  }
  

  /**
   * Checks whether an incoming dependency exists, which has a non written frame for this reactive
   */
  private def needToWaitForIncoming(reactive: Reactive) = {
    reactive.incoming.get.exists { reactive =>
      pipelineFor(reactive).findFrame {
        _ match {
          case Some(frame) => !frame.isWritten
          case None        => false
        }
      }
    }
  }

  /**
   * Checks what to do with the current head: Evaluate it now, later or never
   */
  private def checkEvaluation(reactive: Reactive): PipelinedEvaluationRequest = {
    pipelineFor(reactive).findFrame {
      _ match {
        case Some(frame @ WriteFrame(_, _)) =>
          if (needToWaitForIncoming(reactive))
            EvaluateLater // A concurrent dynamic dependency add is in topological order before reactive, so evaluate this reactive later
          else
            EvaluateNow(frame) // Otherwise evaluate it now
        case Some(frame) => throw new AssertionError("Connot write into dynamic frame")
        case None        => DontEvaluate // frame was removed due to an dynamic dependency drop while waiting, just dont evaluate it
      }
    }
  }

  override def evaluate(head: Reactive) = {

    pipelineFor(head).waitUntilCanWrite(this)

    //println(s"${Thread.currentThread().getId} EVALUATE $head during $this")

    

    checkEvaluation(head) match {
      case DontEvaluate =>
        DoNothing
      case EvaluateLater =>
        assert(pipelineFor(head).hasFrame(this), s"${Thread.currentThread().getId} No frame was created in turn $this for $head")
        assert(assertFrameOrder(head))
        RequeueReactive
      case EvaluateNow(writeFrame) =>
        assert(pipelineFor(head).hasFrame(this), s"${Thread.currentThread().getId} No frame was created in turn $this for $head")
        assert(assertFrameOrder(head))
        assert(Option(pipelineFor(head).needFrame().previous()).map { _.isWritten }.getOrElse(true))
        pipelineFor(head).markTouched
        val queueAction = super.evaluate(head)
        queueAction match {
          case EnqueueDependencies =>
            commitFor(head)
            pipelineFor(head).markWritten
            EnqueueDependencies
          case RequeueReactive =>
          // This reactive will be evaluated once more, so we cannot finish it
          case DoNothing       => assert(false)
        }
        queueAction
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
  }

  private def hasWriteableFrame(pipeline: Pipeline): Boolean = {
    pipeline.findFrame {
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
      val readFrame = markReactiveFramed(source, source => engine.createDynamicReadFrameFrame(this, from = sink, at = source))
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
              println(s"Create frame for $turn at $reactive")
              val frameCreated = turn.markReactiveFramed(reactive, reactive => engine.createFrameAfter(this, turn, reactive))
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

    println(s"Unregister $source as incoming of $sink for $this")

    val sourcePipeline = pipelineFor(source)
    val sourceHasFrame = hasWriteableFrame(sourcePipeline)
    val needToRemoveDep = source.outgoing.get.contains(sink)

    if (needToRemoveDep) {
      val dropFrame = if (!sourceHasFrame) {
        println("Need to create drop frame")
        markReactiveFramed(source, source => pipelineFor(source).createDynamicDropFrame(sink))
      } else {
        val writeFrame = pipelineFor(source).findFrame { _.get } // Can use this frame if it is marked written? Should not make a difference
        // But then it need to be written already because it must evaluated first
        // assert(writeFrame.isWritten, "Frame of an incoming dependency needs to be evaluated first")
        writeFrame
      }

      assert(!dropFrame.isWritten)
      assert(pipelineFor(source).hasFrame, s"No drop frame for $this at $source ")

      dropFrame.markTouched()

      println(s"Remove sink $sink from source $source for $this")
      println(sourcePipeline.getPipelineFrames())
      source.outgoing.transform { _ - sink }
      val writeFramesAfterDrop = pipelineFor(source).writeFramesAfter(dropFrame)
      writeFramesAfterDrop.foreach { frame => source.outgoing.transform(_ - sink)(frame.turn) }

      // Cannot simply remove sink from the queue of the turns because sink may be reachable
      // though another valid path, nevertheless, the turns could wait for this frame already

      val turnsAfterDynamicDrop = writeFramesAfterDrop.map(_.turn.asInstanceOf[PipeliningTurn])
      println(s"Turn after dynamic drop turnsAfterDynamicDrop")

      if (turnsAfterDynamicDrop.nonEmpty) {
        // Queue based create frames at reachable reactives
        var deframedReactives = Set[Reactive](source)
        val queue = new LevelQueue
        queue.enqueue(-1)(sink)
        queue.evaluateQueue { reactive =>
          var frameRemoved = false
          for (turn <- turnsAfterDynamicDrop) {
            val pipeline = pipelineFor(reactive)
            val frameToRemove = pipeline.needFrame()
            assert(pipelineFor(reactive).hasFrame(this))
            val incomings = 
            if (reactive.incoming.isInstanceOf[BlockingPipelineBuffer[Set[Reactive]]]) {
            reactive.incoming.asInstanceOf[BlockingPipelineBuffer[Set[Reactive]]].forceGet(turn)
            } else {
              reactive.incoming.get(turn)
            }
            if (incomings.filter(pipelineFor(_).hasFrame(turn)).diff(deframedReactives).isEmpty) {
              println(s"Remove frame for $turn at $reactive")
              turn.asInstanceOf[PipeliningTurn].markReactiveUnframed(reactive, reactive => pipeline.deleteFrames(turn))
              deframedReactives += reactive
              frameRemoved = true
            }
          }
          // Only need to continue created frames, if one was created
          if (frameRemoved)
            reactive.outgoing.get.foreach { queue.enqueue(-1) }
        }

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
    createFrames(initialWrites)
  }

  override def commitPhase(): Unit = {
    //Commit the rest (no buffers, but some tests inject something)
    super.commitPhase()

  }

  override def releasePhase(): Unit = {
    engine.turnCompleted(this)
  }

  protected[pipelining] def markMissingReactives() = {
    readFramedReactive { framedReactives =>
      println(s"Mark missing reactives for $this: $framedReactives")
      framedReactives.foreach { reactive =>
        val frame = pipelineFor(reactive).needFrame();
        if (!frame.isWritten) {
          commitFor(reactive)
          frame.markWritten
        }
      }
    }
  }

  protected[pipelining] def removeFrames() = {
    doFramedWriteLocked { framedReactives =>
      {
        framedReactives.foreach { pipelineFor(_).removeFrames }
        assert(framedReactives.forall { !pipelineFor(_).hasFrame(this) })
        Set.empty
      }
    }

  }

  override def toString = {
    s"PipeliningTurn(${super.toString})"
  }

  override def >=(other: Turn) = {
    other == this || engine.waitsOn(this, other.asInstanceOf[PipeliningTurn])
  }

  def <(other: Turn) = {
    !engine.waitsOn(this, other.asInstanceOf[PipeliningTurn])
  }

}