package rescala.pipelining

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import rescala.graph.Committable
import rescala.graph.Reactive
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
import rescala.graph.Frame
import scala.actors.threadpool.locks.ReentrantLock
import java.util.concurrent.locks.LockSupport
import java.util.concurrent.Semaphore

object PipeliningTurn {

  sealed abstract class PipelinedEvaluationRequest
  case class EvaluateNow(writeFrame: Frame[BufferFrameContent]) extends PipelinedEvaluationRequest
  case object EvaluateLater extends PipelinedEvaluationRequest
  case object DontEvaluate extends PipelinedEvaluationRequest

}

class PipeliningTurn(override val engine: PipelineEngine, randomizeDeps: Boolean = false)
  extends TurnImpl
  with PropagateNoChanges
  with ParallelFrameCreator {

  import Pipeline._
  import PipeliningTurn._
  import LogUtils._

  /**
   * Remember all reactives for which a frame was created during this turn
   */
  @volatile
  private var framedReactives = Set[Reactive]()
  private[pipelining] val framedReactivesLock = new ReentrantReadWriteLock()

  val thread = Thread.currentThread()

  override val levelQueue = new AwakingLevelQueue(this)

  private val needPropagateCheck = new Semaphore(1)

  protected[pipelining] def needContinue() = {
    needPropagateCheck.release()
  }

  private def waitUntilContinue() = {
    needPropagateCheck.acquire()
  }

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
  override implicit def currentTurn: PipeliningTurn = this

  override def create[T <: Reactive](dependencies: Set[Reactive], dynamic: Boolean)(f: => T): T = {
    val reactive = f

    markReactiveFramed(reactive, Pipeline(_).createFrame(this))
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
  private def needToWaitForIncoming(head: Reactive) = {
    Pipeline(head).needFrame().isWritten || // TODO this is wait because an admit might readd a dependency to the queue while the head was removed
      head.incoming.get.exists { reactive =>
        head.level.get <= reactive.level.get || // TODO Check whether level is enough
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
    pipelineFor(reactive).findFrame { x => x } match {
      case Some(frame) =>
        if (needToWaitForIncoming(reactive))
          EvaluateLater // A concurrent dynamic dependency add is in topological order before reactive, so evaluate this reactive later
        else
          EvaluateNow(frame) // Otherwise evaluate it now
      case None => DontEvaluate // frame was removed due to an dynamic dependency drop while waiting, just dont evaluate it
    }
  }

  override def evaluate(head: Reactive) = {

    Pipeline(head).waitUntilCanRead(this)
    pipelineFor(head).waitUntilCanWrite(this)

    // println(s"EVALUATE $head during $this at ${head.level.get}")
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
        // val queueAction = super.evaluate(head)
        val result = head.reevaluate()
        val (hasChanged, newLevel, queueAction) = calculateQueueAction(head, result)

        Pipeline(head).lockDynamic {
          requeue(head, hasChanged, newLevel, queueAction)
          queueAction match {
            case EnqueueDependencies =>
              commitFor(head)
              pipelineFor(head).markWritten
              EnqueueDependencies
            case RequeueReactive =>
            // This reactive will be evaluated once more, so we cannot finish it
            case DoNothing       => assert(false)
          }
          /*assert({
          head.outgoing.get.foreach { r =>
            assert(pipelineFor(r).hasFrame, s"No frame at ougoing $r of $head during $this")
          }
          true
        })*/
          //     println(s"EVALUATE $head completed ")
          queueAction
        }
    }
  }

  override def evaluateNoChange(head: Reactive): QueueAction = {
    println(s"NOT EVALUATE $head")
    pipelineFor(head).waitUntilCanWrite(this)
    if (!pipelineFor(head).needFrame().isWritten) {
      commitFor(head)
      pipelineFor(head).markWritten
    }
    requeue(head, changed = false, level = -1, action = EnqueueDependencies)
    EnqueueDependencies
  }

  /**
   * Evaluates the queue as long as a new element is added. This may occur multiple times
   * because of a dynamic dependency discovery by another turn which is admitted to this turn
   * but the usual propagation of this turn has already completed.
   * This method returns only if the current turn can be removed (if it is the head turn in the turn order in the engine)
   */
  override def propagationPhase(): Unit = {
    // The first time, waitUntilContinue returns
    // If the engine detects, that the turn can be removed, the level queue is empty
    // But the engine lets waitUntilContinue return again
    while ({
      waitUntilContinue()
      !(levelQueue.isEmpty() && engine.canTurnBeRemoved(this))
    }) {

      levelQueue.evaluateQueue(evaluate, evaluateNoChange)
      // Lets waitUntilContinue return, if a new element is enqueued
      levelQueue.awakeOnNewElement()
    }
    assert(levelQueue.isEmpty())
    assert(engine.canTurnBeRemoved(this))
  }

  private def commitFor(head: Reactive): Unit = {
    commitFor(pipelineFor(head))
  }

  private def commitFor(head: Pipeline): Unit = {
    assert(Option(head.needFrame().previous()).map { _.isWritten }.getOrElse(true))
    val buffersToCommit = head.createdBuffers.asInstanceOf[Set[Committable]]
    buffersToCommit.foreach { _.commit }
  }

  private def getWriteableFrame(pipeline: Pipeline): Frame[BufferFrameContent] = {
    val frame = pipeline.frame(this)
    if (frame.turn == this)
      frame
    else if (frame.next() == null) {
      // frame is not for this turn, so before in turn order
      // but there is no appropiate frame to write into, so create one

      markReactiveFramed(pipeline.reactive, _ => {
        println(s"Create frame for dynamic during $this")
        pipeline.createFrame { frame =>
          // If the previous frame was written, this one needs to be too (outgoings are allowed to be changed in written frames)
          if (frame.previous().isWritten) {
            commitFor(pipeline)
            frame.markWritten()
          }
        }(this)
      })
      pipeline.needFrame()
    } else {
      frame.next()
    }
  }

  private def getAffectedTurns(reactive: Reactive, dynamicFrame: Frame[BufferFrameContent]) = {
    val firstFrame = if (dynamicFrame.turn == this) dynamicFrame.next() else dynamicFrame
    val affectedFrames = pipelineFor(reactive).forWriteFramesAfter(firstFrame) { _ => }
    affectedFrames.map(_.turn.asInstanceOf[PipeliningTurn])
  }

  override def register(sink: Reactive)(source: Reactive): Unit = {
    val needToAddDep = !source.outgoing.get.contains(sink)
    val sourcePipeline = pipelineFor(source)
    //  assert(sourcePipeline.frame.isWritten)
    if (needToAddDep) {
      println(s"Register for $this $source as source for $sink")

      val turnsAfterDynamicRead = Pipeline(source).lockDynamic {

        val dropFrame: Frame[BufferFrameContent] = getWriteableFrame(sourcePipeline)
        println(s"Readframe: turn = ${sourcePipeline.frame(this).turn}")
        println(s"DropFrame: turn = $dropFrame")

        assert(this <= dropFrame.turn)

        // Need to create frames for the turns after the dynamic read at the new nodes
        val turnsAfterDynamicRead = getAffectedTurns(source, dropFrame)
        println(s"Turns after dynamic read $turnsAfterDynamicRead")
        assert(turnsAfterDynamicRead.forall { this < _ })

        // This writes in the dynamic read frame, but only in outgoings
        source.outgoing.transform { _ + sink }(dropFrame.turn)

        // this defect in frames after this frames we need to repair
        // Get all write frames after the read frame -> need to propagate the new dependency to them

        turnsAfterDynamicRead.foreach(turn => {
          // frame.turn.asInstanceOf[PipeliningTurn].createFramesLock.synchronized {
          source.outgoing.transform(_ + sink)(turn)
          //  }
        })

        // Add the new nodes in the queue of the turns
        turnsAfterDynamicRead.foreach { turn =>
          val sourceFrame = Pipeline(source).needFrame()(turn)
          if (sourceFrame.isWritten && sourceFrame.isTouched) {
            println(s"During $this admit $sink in turn $turn")
            turn.admit(sink)
          }

        }
        turnsAfterDynamicRead
      }

      if (turnsAfterDynamicRead.nonEmpty) {
        // Queue based create frames at reachable reactives
        val queue = new LevelQueue
        queue.enqueue(-1)(sink)
        queue.evaluateQueue { reactive =>
          val pipeline = Pipeline(reactive)
          pipeline.lockDynamic {
            if (pipeline.hasFrame) {
              var anyFrameCreated = false
              for (turn <- turnsAfterDynamicRead) {
                // Need to grab the framing lock for turn before(!) grabbing a pipeline lock
                turn.doFramedWriteLocked { reactives =>

                  assert(pipelineFor(reactive).hasFrame(this), s"Can only create frames due to an dependency add for frames which are already covered for $this which $reactive is not (for turns $turnsAfterDynamicRead)")
                  assert(!pipelineFor(reactive).needFrame().isWritten)

                  println(s"Create frame for $turn at $reactive during $this")
                  val frameCreated = engine.createFrameAfter(this, turn, reactive)
                  anyFrameCreated ||= frameCreated
                  reactives + reactive
                }
                // Only need to continue created frames, if one was created
                if (anyFrameCreated)
                  reactive.outgoing.get(this).foreach { queue.enqueue(-1) }
              }
            }
          }
        }
      }

      val sinkPipeline = Pipeline(sink)
      sinkPipeline.lockDynamic {

        if (sink.incoming.isInstanceOf[PipelineBuffer[_]]) {
          val sinkFrames = sinkPipeline.forWriteFramesAfter(sinkPipeline.needFrame()) { _ => }
          sinkFrames.foreach { frame =>
            sink.incoming.asInstanceOf[BlockingPipelineBuffer[Set[Reactive]]].forceTransform { _ + source }(frame.turn)
          }
        }
      }
      
      println(s"Completed register during $this at $source: ${Pipeline(source).getPipelineFramesWithStable()}");

    }
  }

  override def unregister(sink: Reactive)(source: Reactive): Unit = {

    println(s"Unregister $source as incoming of $sink for $this")

    val sourcePipeline = pipelineFor(source)
    val needToRemoveDep = source.outgoing.get.contains(sink)

    if (needToRemoveDep) {

      val turnsAfterDynamicDrop = Pipeline(source).lockDynamic {

        val dropFrame = getWriteableFrame(sourcePipeline)

        println(s"Remove sink $sink from source $source for $this")

        source.outgoing.transform { _ - sink }(dropFrame.turn)

        // Cannot simply remove sink from the queue of the turns because sink may be reachable
        // though another valid path, nevertheless, the turns could wait for this frame already

        val turnsAfterDynamicDrop = getAffectedTurns(source, dropFrame)
        turnsAfterDynamicDrop.foreach { source.outgoing.transform { _ - sink }(_) }
        println(s"Turn after dynamic drop $turnsAfterDynamicDrop")
        turnsAfterDynamicDrop
      }

      if (sink.incoming.isInstanceOf[PipelineBuffer[_]]) {
        val sinkPipeline = pipelineFor(sink)
        val sinkFrames = sinkPipeline.forWriteFramesAfter(sinkPipeline.needFrame()) { _ => }
        sinkFrames.foreach { frame =>
          sink.incoming.asInstanceOf[BlockingPipelineBuffer[Set[Reactive]]].forceTransform { _ - source }(frame.turn)
        }
      }

      assert(engine.isActive(this))
      //  assert(engine.isActive(dropFrame.turn))
      assert(turnsAfterDynamicDrop.forall { engine.isActive(_) })

      if (turnsAfterDynamicDrop.nonEmpty) {
        // Queue based remove frames at reachable reactives
        val queue = new LevelQueue
        queue.enqueue(-1)(sink)
        queue.evaluateQueue { reactive =>

          var frameRemoved = false
          for (turn <- turnsAfterDynamicDrop) {
            val pipeline = pipelineFor(reactive)
            pipeline.lockDynamic {
              if (pipeline.hasFrame) {
                assert(pipelineFor(reactive).hasFrame(this))
                val incomings = if (reactive.incoming.isInstanceOf[BlockingPipelineBuffer[_]])
                  reactive.incoming.asInstanceOf[BlockingPipelineBuffer[Set[Reactive]]].forceGet(turn)
                else
                  reactive.incoming.get(turn)
                if (incomings.filter(pipelineFor(_).hasFrame(turn)).isEmpty) {
                  println(s"Remove frame for $turn at $reactive")
                  turn.asInstanceOf[PipeliningTurn].markReactiveUnframed(reactive, reactive => pipeline.deleteFrames(turn))
                  frameRemoved = true
                }
              }
            }
          }
          // Only need to continue created frames, if one was created
          if (frameRemoved)
            reactive.outgoing.get.foreach { queue.enqueue(-1) }
        }

      }

    }

  }

  override def lockPhase(initialWrites: List[Reactive]): Unit = {
    println(s"$this starts framing phase")
    createFrames(initialWrites)
    println(s"$this completed framing phase")
  }

  override def commitPhase(): Unit = {
    assert(engine.canTurnBeRemoved(this))
    //Commit the rest (no buffers, but some tests inject something)
    super.commitPhase()
  }

  override def releasePhase(): Unit = {
    assert(engine.canTurnBeRemoved(this))
    markMissingReactives()
    removeFrames()
    engine.turnCompleted(this)
  }

  private def markMissingReactives() = {
    assert(levelQueue.isEmpty())
    assert(engine.canTurnBeRemoved(this))
    // We are the first turn, no one will change our frames anymore
    this.framedReactives.foreach { reactive =>
      val frame = pipelineFor(reactive).needFrame();
      if (!frame.isWritten) {
        pipelineFor(reactive).waitUntilCanWrite
        commitFor(reactive)
        frame.markWritten
      }
    }
  }

  private def removeFrames() = {
    // We are the first turn, no one will change our frames anymore
    this.framedReactives.foreach { reactive =>
      val pipeline = pipelineFor(reactive)
      pipeline.lockDynamic {
        pipeline.removeFrames
      }
    }
    assert(framedReactives.forall { !pipelineFor(_).hasFrame(this) })
    this.framedReactives = Set.empty
  }

  override def toString = {
    s"PipeliningTurn(${super.toString})"
  }
  
  def >(other: Turn) = {
    engine.waitsOn(this, other.asInstanceOf[PipeliningTurn])
  }

  def >=(other: Turn) = {
    other == this || this > other
  }

  def <(other: Turn) = {
    ! (this >= other)
  }

  def <=(other: Turn) = {
    ! (this > other)
  }

}