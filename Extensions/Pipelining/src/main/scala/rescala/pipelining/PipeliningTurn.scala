package rescala.pipelining

import java.util.concurrent.Semaphore
import java.util.concurrent.locks.ReentrantReadWriteLock

import rescala.graph.{Pulse, Reactive}
import rescala.pipelining.Pipeline._
import rescala.pipelining.PipeliningTurn._
import rescala.pipelining.propagation._
import rescala.pipelining.util.LogUtils._
import rescala.propagation._
import rescala.graph.ReadWritePulseStruct
import rescala.twoversion.Committable


private[pipelining] object PipeliningTurn {

  sealed abstract class PipelinedEvaluationRequest
  case class EvaluateNow(writeFrame: Frame[BufferFrameContent]) extends PipelinedEvaluationRequest
  case object EvaluateLater extends PipelinedEvaluationRequest
  case object DontEvaluate extends PipelinedEvaluationRequest

}

class PipeliningTurn(val engine: PipelineEngine, randomizeDeps: Boolean = false)
  extends PipelinePropagationImpl
    with PropagateNoChanges
    with ParallelFrameCreator {

  /** used to create state containers of each reactive */
  override type S = PipelineStruct.type


  /** used to create state containers of each reactive */
  override def makeStructState[P, R](initialValue: Pulse[P], transient: Boolean, initialIncoming: Set[R]): S#StructType[P, R] = new PipelineSporeP[P, R](initialValue, transient, initialIncoming)


  /**
    * Remember all reactives for which a frame was created during this turn
    */
  @volatile
  private var framedPipelines = Set[Pipeline]()
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

  private def doFramedWriteLocked(op: Set[Pipeline] => Set[Pipeline]): Unit = {
    val writeLock = framedReactivesLock.writeLock()
    writeLock.lock()
    framedPipelines = op(framedPipelines)
    writeLock.unlock()
  }

  protected[pipelining] def markReactiveFramed[T](pipeline: Pipeline, createFrame: Pipeline => T): T = {
    var frame = null.asInstanceOf[T]
    doFramedWriteLocked(pipelines => {
      frame = createFrame(pipeline)
      pipelines + pipeline
    })
    frame
  }

  private def markReactiveUnframed(pipeline: Pipeline, removeFrame: Pipeline => Unit) = {
    doFramedWriteLocked(pipelines => {
      removeFrame(pipeline)
      pipelines - pipeline
    })
  }
  override implicit def currentTurn: PipeliningTurn = this

  override def create[T <: Reactive[S]](dependencies: Set[Reactive[S]], dynamic: Boolean)(f: => T): T = {
    val reactive = f

    markReactiveFramed(reactive.state.pipeline, _.createFrame(this))
    ensureLevel(reactive, dependencies)
    if (dynamic) {
      evaluate(reactive)
    }
    else {
      dependencies.foreach(discover(reactive))
      evaluateNoChange(reactive)
    }
    reactive
  }

  private def assertFrameOrder(head: Reactive[S]): Boolean = {
    var frameFound = false
    head.state.pipeline.foreachFrameTopDown { frame =>
      if (frame.turn == this) {
        frameFound = true
      }
      else if (!frameFound) {
        assert(this >= frame.turn)
        assert(frame.isWritten)
      }
      else {
        assert(frame.turn >= this)
      }
    }
    true
  }

  /**
    * Checks whether an incoming dependency exists, which has a non written frame for this reactive
    */
  private def needToWaitForIncoming(head: Reactive[S]) = {
    Pipeline(head).needFrame().isWritten || // TODO this is wait because an admit might readd a dependency to the queue while the head was removed
      head.state.incoming.exists { reactive =>
        head.state.level <= reactive.state.level || // TODO Check whether level is enough
          pipelineFor(reactive).findFrame {
            case Some(frame) => !frame.isWritten
            case None => false
          }
      }
  }

  /**
    * Checks what to do with the current head: Evaluate it now, later or never
    */
  private def checkEvaluation(reactive: Reactive[S]): PipelinedEvaluationRequest = {
    pipelineFor(reactive).findFrame { x => x } match {
      case Some(frame) =>
        if (needToWaitForIncoming(reactive))
          EvaluateLater // A concurrent dynamic dependency add is in topological order before reactive, so evaluate this reactive later
        else
          EvaluateNow(frame) // Otherwise evaluate it now
      case None => DontEvaluate // frame was removed due to an dynamic dependency drop while waiting, just dont evaluate it
    }
  }

  override protected def requeue(head: Reactive[S], changed: Boolean, level: Int, action: QueueAction): Unit = action match {
    case EnqueueDependencies => head.state.outgoing.foreach(levelQueue.enqueue(level, changed))
    case RequeueReactive => levelQueue.enqueue(level, changed)(head)
    case DoNothing =>
  }

  override def evaluate(head: Reactive[S]) = {

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
        assert(Option(pipelineFor(head).needFrame().previous()).fold(true)(_.isWritten))
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
            case DoNothing => assert(false)
          }
          /*assert({
          head.bud.outgoing.get.foreach { r =>
            assert(pipelineFor(r).hasFrame, s"No frame at ougoing $r of $head during $this")
          }
          true
        })*/
          //     println(s"EVALUATE $head completed ")
          queueAction
        }
    }
  }

  override def evaluateNoChange(head: Reactive[S]): QueueAction = {
    log(s"NOT EVALUATE $head")
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
    while ( {
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

  private def commitFor(head: Reactive[S]): Unit = {
    commitFor(pipelineFor(head))
  }

  private def commitFor(head: Pipeline): Unit = {
    assert(Option(head.needFrame().previous()).fold(true)(_.isWritten))
    val buffersToCommit = head.createdBuffers.asInstanceOf[Set[Committable]]
    buffersToCommit.foreach {_.commit}
  }

  private def getWriteableFrame(pipeline: Pipeline): Frame[BufferFrameContent] = {
    val frame = pipeline.frame(this)
    if (frame.turn == this)
      frame
    else if (frame.next() == null) {
      // frame is not for this turn, so before in turn order
      // but there is no appropiate frame to write into, so create one

      markReactiveFramed(pipeline, _ => {
        log(s"Create frame for dynamic during $this")
        pipeline.createFrame { frame =>
          // If the previous frame was written, this one needs to be too (outgoings are allowed to be changed in written frames)
          if (frame.previous().isWritten) {
            commitFor(pipeline)
            frame.markWritten()
          }
        }(this)
      })
      pipeline.needFrame()
    }
    else {
      frame.next()
    }
  }

  private def getAffectedTurns(reactive: Reactive[S], dynamicFrame: Frame[BufferFrameContent]) = {
    val firstFrame = if (dynamicFrame.turn == this) dynamicFrame.next() else dynamicFrame
    val affectedFrames = pipelineFor(reactive).forWriteFramesAfter(firstFrame) { _ => }
    affectedFrames.map(_.turn)
  }

  def discover(sink: Reactive[S])(source: Reactive[S]): Unit = {
    val needToAddDep = !source.state.outgoing.contains(sink)
    val sourcePipeline = pipelineFor(source)
    //  assert(sourcePipeline.frame.isWritten)
    if (needToAddDep) {
      log(s"Register for $this $source as source for $sink")

      val turnsAfterDynamicRead = Pipeline(source).lockDynamic {

        val dropFrame: Frame[BufferFrameContent] = getWriteableFrame(sourcePipeline)
        log(s"Readframe: turn[S = ${sourcePipeline.frame(this).turn}")
        log(s"DropFrame: turn[S = $dropFrame")

        assert(this <= dropFrame.turn)

        // Need to create frames for the turns after the dynamic read at the new nodes
        val turnsAfterDynamicRead = getAffectedTurns(source, dropFrame)
        log(s"Turns after dynamic read $turnsAfterDynamicRead")
        assert(turnsAfterDynamicRead.forall {this < _})

        // This writes in the dynamic read frame, but only in outgoings
        source.state.discover(sink)(dropFrame.turn)

        // this defect in frames after this frames we need to repair
        // Get all write frames after the read frame -> need to propagate the new dependency to them

        turnsAfterDynamicRead.foreach(turn => {
          // frame.turn.asInstanceOf[PipeliningTurn].createFramesLock.synchronized {
          source.state.discover(sink)(turn)
          //  }
        })

        // Add the new nodes in the queue of the turns
        turnsAfterDynamicRead.foreach { turn =>
          val sourceFrame = Pipeline(source).needFrame()(turn)
          if (sourceFrame.isWritten && sourceFrame.isTouched) {
            log(s"During $this admit $sink in turn $turn")
            turn.admit(sink)
          }

        }
        turnsAfterDynamicRead
      }

      if (turnsAfterDynamicRead.nonEmpty) {
        // Queue based create frames at reachable reactives
        val queue = new PipelineQueue()
        queue.enqueue(-1)(sink)
        queue.evaluateQueue { reactive =>
          val pipeline = Pipeline(reactive)
          pipeline.lockDynamic {
            if (pipeline.hasFrame) {
              var anyFrameCreated = false
              for (turn <- turnsAfterDynamicRead) {
                // Need to grab the framing lock for turn before(!) grabbing a pipeline lock
                turn.doFramedWriteLocked { pipelines =>

                  assert(pipelineFor(reactive).hasFrame(this), s"Can only create frames due to an dependency add for frames which are already covered for $this which $reactive is not (for turns $turnsAfterDynamicRead)")
                  assert(!pipelineFor(reactive).needFrame().isWritten)

                  log(s"Create frame for $turn at $reactive during $this")
                  val frameCreated = pipeline.createFrameAfter(this, turn)
                  anyFrameCreated ||= frameCreated
                  pipelines + Pipeline(reactive)
                }
                // Only need to continue created frames, if one was created
                if (anyFrameCreated)
                  reactive.state.outgoing(this).foreach {queue.enqueue(-1)}
              }
            }
          }
        }
      }

      val sinkPipeline = Pipeline(sink)
      sinkPipeline.lockDynamic {

        if (sink.state.incoming.isInstanceOf[PipelineBuffer[_]]) {
          val sinkFrames = sinkPipeline.forWriteFramesAfter(sinkPipeline.needFrame()) { _ => }
          sinkFrames.foreach { frame =>
            sink.state.incoming.asInstanceOf[BlockingPipelineBuffer[Set[Reactive[S]]]].forceTransform {_ + source}(frame.turn)
          }
        }
      }

      log(s"Completed register during $this at $source: ${Pipeline(source).getPipelineFramesWithStable()}");

    }
  }

  def drop(sink: Reactive[S])(source: Reactive[S]): Unit = {

    log(s"Unregister $source as incoming of $sink for $this")

    val sourcePipeline = pipelineFor(source)
    val needToRemoveDep = source.state.outgoing.contains(sink)

    if (needToRemoveDep) {

      val turnsAfterDynamicDrop = Pipeline(source).lockDynamic {

        val dropFrame = getWriteableFrame(sourcePipeline)

        log(s"Remove sink $sink from source $source for $this")

        source.state.drop(sink)(dropFrame.turn)

        // Cannot simply remove sink from the queue of the turns because sink may be reachable
        // though another valid path, nevertheless, the turns could wait for this frame already

        val turnsAfterDynamicDrop = getAffectedTurns(source, dropFrame)
        turnsAfterDynamicDrop.foreach {source.state.drop(sink)(_)}
        log(s"Turn after dynamic drop $turnsAfterDynamicDrop")
        turnsAfterDynamicDrop
      }

      if (sink.state.incoming.isInstanceOf[PipelineBuffer[_]]) {
        val sinkPipeline = pipelineFor(sink)
        val sinkFrames = sinkPipeline.forWriteFramesAfter(sinkPipeline.needFrame()) { _ => }
        sinkFrames.foreach { frame =>
          sink.state.incoming.asInstanceOf[BlockingPipelineBuffer[Set[Reactive[S]]]].forceTransform {_ - source}(frame.turn)
        }
      }

      assert(engine.isActive(this))
      //  assert(engine.isActive(dropFrame.turn))
      assert(turnsAfterDynamicDrop.forall {engine.isActive})

      if (turnsAfterDynamicDrop.nonEmpty) {
        // Queue based remove frames at reachable reactives
        val queue = new PipelineQueue()
        queue.enqueue(-1)(sink)
        queue.evaluateQueue { reactive =>

          var frameRemoved = false
          for (turn <- turnsAfterDynamicDrop) {
            val pipeline = pipelineFor(reactive)
            pipeline.lockDynamic {
              if (pipeline.hasFrame) {
                assert(pipelineFor(reactive).hasFrame(this))
                val incomings = reactive.state.incomingForceGet(turn)
                if (!incomings.exists(pipelineFor(_).hasFrame(turn))) {
                  log(s"Remove frame for $turn at $reactive")
                  turn.asInstanceOf[PipeliningTurn].markReactiveUnframed(pipeline, _.deleteFrames(turn))
                  frameRemoved = true
                }
              }
            }
          }
          // Only need to continue created frames, if one was created
          if (frameRemoved)
            reactive.state.outgoing.foreach {queue.enqueue(-1)}
        }

      }

    }

  }

  override def preparationPhase(initialWrites: Traversable[Reactive[S]]): Unit = {
    log(s"$this starts framing phase")
    createFrames(initialWrites)
    log(s"$this completed framing phase")
    super.preparationPhase(initialWrites)
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
    this.framedPipelines.foreach { pipelines =>
      val frame = pipelines.needFrame()
      if (!frame.isWritten) {
        pipelines.waitUntilCanWrite
        commitFor(pipelines)
        frame.markWritten()
      }
    }
  }

  private def removeFrames() = {
    // We are the first turn, no one will change our frames anymore
    this.framedPipelines.foreach { pipeline =>
      pipeline.lockDynamic {
        pipeline.removeFrames
      }
    }
    assert(framedPipelines.forall {!_.hasFrame(this)})
    this.framedPipelines = Set.empty
  }

  override def toString = {
    s"PipeliningTurn(${super.toString})"
  }

  def >(other: Turn[S]) = {
    engine.waitsOn(this, other.asInstanceOf[PipeliningTurn])
  }

  def >=(other: Turn[S]) = {
    other == this || this > other
  }

  def <(other: Turn[S]) = {
    !(this >= other)
  }

  def <=(other: Turn[S]) = {
    !(this > other)
  }

}
