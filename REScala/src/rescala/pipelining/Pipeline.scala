package rescala.pipelining

import rescala.turns.Turn
import scala.collection.immutable.Queue
import scala.annotation.tailrec
import rescala.graph._
import java.util.concurrent.locks.ReentrantLock

object Pipeline {
  protected[pipelining] def pipelineFor(at: Reactive) = at.pipeline
  protected[pipelining] def apply(at: Reactive) = at.pipeline
}

class Pipeline(val reactive: Reactive) {

  protected[this]type Content = BufferFrameContent;

  private type CFrame = Frame[Content]

  protected[this] def initialStableFrame: Content = new BufferFrameContent
  protected[this] def duplicate(content: Content, newTurn: Turn): Content = content.duplicate(newTurn)
  protected[this] var stableFrame: Frame[BufferFrameContent] = Frame[BufferFrameContent](null, this)
  stableFrame.content = initialStableFrame
  stableFrame.markWritten()
  protected[rescala] def getStableFrame() = stableFrame

  protected[this] var queueTail: CFrame = stableFrame

  private object pipelineLock

  private[pipelining] object dynamicLock 
  
  protected[pipelining] def lockDynamic[A](op : => A) : A = dynamicLock.synchronized {
    op
  }

  private def lockPipeline[A](op: => A): A = pipelineLock.synchronized {
    op
  }

  private def queueHead() = stableFrame.next()

  private def insertHead(insert: CFrame) = {
    insert.insertAfter(stableFrame)
    if (queueTail == stableFrame)
      queueTail = insert
  }

  private def insertAfter(insert: CFrame, after: CFrame) = {
    val insertEnd = after == queueTail
    insert.insertAfter(after)
    if (insertEnd)
      queueTail = insert
  }

  private def deleteFrame(frame: CFrame) = {
    val newTail = if (queueTail == frame) frame.previous() else queueTail
    frame.removeFrame()
    queueTail = newTail
  }

  private def replaceStableFrame() = lockPipeline{
    /* val frameToRemove = stableFrame.next()
    val newStableFrameContent = frameToRemove.content
    assert(frameToRemove.isWritten)
    val newStableFrame = Frame[Content](null, this)
    newStableFrame.oldTurn = frameToRemove.turn
    newStableFrame.content = newStableFrameContent
    newStableFrame.markWritten()
    stableFrame.removeFrame()
    val pipelineRest = frameToRemove.next
    frameToRemove.removeFrame()
    if (pipelineRest == null)
      queueTail = newStableFrame
    else 
      pipelineRest.insertAfter(newStableFrame)
    stableFrame = newStableFrame*/
    val newStableFrame = stableFrame.next
    newStableFrame.oldTurn = newStableFrame.turn
    newStableFrame.turn = null
    stableFrame.removeFrame()
    stableFrame = newStableFrame
  }

  protected[pipelining] var createdBuffers: Set[PipelineBuffer[_]] = Set()

  protected[pipelining] def createBlockingBuffer[T](initval: T, commitStrategy: (T, T) => T): BlockingPipelineBuffer[T] = {
    assert(queueHead == null)
    val newBuffer = new BlockingPipelineBuffer(this, commitStrategy)
    val holder = ValueHolder.initStable(initval, newBuffer)
    stableFrame.content.values :+= holder
    createdBuffers += newBuffer
    newBuffer
  }

  protected[pipelining] def createNonblockingBuffer[T](initval: T, commitStrategy: (T, T) => T): NonblockingPipelineBuffer[T] = {
    assert(queueHead == null)
    val newBuffer = new NonblockingPipelineBuffer(this, commitStrategy)
    val holder = ValueHolder.initStable(initval, newBuffer)
    stableFrame.content.values :+= holder
    createdBuffers += newBuffer
    newBuffer
  }

  private def assertTurnOrder() = {
    var currentFrame = stableFrame
    assert(currentFrame != null)
    var nextFrame = stableFrame.next
    while (nextFrame != null) {
      assert(currentFrame != null)
      assert(nextFrame != null)
      assert(currentFrame.turn == null || currentFrame.turn < nextFrame.turn, s"Wrong turn order: ${currentFrame.turn} not < than ${nextFrame.turn}:  ${getPipelineFrames()}")
      currentFrame = nextFrame
      nextFrame = nextFrame.next
    }
    true
  }

  // Access for testing
  protected[rescala] final def getPipelineFrames() = lockPipeline {
    def makeQueue(head: CFrame, queue: Queue[CFrame]): Queue[CFrame] = {
      if (head == null)
        queue
      else
        makeQueue(head.next(), queue :+ head)
    }
    makeQueue(queueHead, Queue())
  }
  // Access for testing
  protected[rescala] final def getPipelineFramesWithStable() = lockPipeline {
    def makeQueue(head: CFrame, queue: Queue[CFrame]): Queue[CFrame] = {
      if (head == null)
        queue
      else
        makeQueue(head.next(), queue :+ head)
    }
    makeQueue(stableFrame, Queue())
  }

  protected[rescala] def findFrame[T](find: Option[CFrame] => T)(implicit turn: PipeliningTurn): T = lockPipeline {
    @tailrec
    def findFrame(tail: CFrame = queueTail): Option[CFrame] = {
      if (tail == null)
        None
      else if (tail.turn eq turn)
        Some(tail)
      else findFrame(tail.previous())
    }
    val selectedFrame = findFrame()
    find(selectedFrame)
  }

  private def findFrame[T](found: CFrame => T, notFound: => T)(implicit turn: PipeliningTurn): T = {
    findFrame(_ match {
      case Some(d) => found(d)
      case None    => notFound
    })
  }

  protected[rescala] def needFrame[T](op: CFrame => T = { x: CFrame => x })(implicit turn: PipeliningTurn): T = {
    findFrame(_ match {
      case Some(d) => op(d)
      case None    => throw new AssertionError(s"No frame found for $turn at ${this.reactive}: ${this.getPipelineFrames()}")
    })
  }

  protected[pipelining] def frame(implicit turn: PipeliningTurn): CFrame = lockPipeline {
    @tailrec
    def findBottomMostFrame(tail: CFrame): CFrame = {
      if (turn >= tail.turn)
        tail
      else
        findBottomMostFrame(tail.previous())
    }

    // Local: if the turn itself is found, it is the bottom most frame => no need to sync
    val bottomMostWaitingFrame: CFrame = findFrame(x => x).getOrElse(findBottomMostFrame(queueTail))
    bottomMostWaitingFrame
  }

  protected[pipelining] def frame[T](f: Content => T = { x: Content => x })(implicit turn: PipeliningTurn): T = {

    f(frame.content)
  }

  protected[rescala] def waitUntilCanWrite(implicit turn: PipeliningTurn): Unit = {
    waitUntilCanRead
  }

  protected[rescala] def waitUntilCanRead(implicit turn: PipeliningTurn): Unit = {
    Frame.awaitUntilWritten(pipelineLock, this,  turn)
    assert(if(frame(turn).turn == turn) frame(turn).previous().isWritten else frame(turn).isWritten )
  }

  protected[rescala] def hasFrame(implicit turn: PipeliningTurn): Boolean = {
    findFrame(_ => true, false)
  }

  protected[rescala] def ifFrame[A](doForFrame: CFrame => A)(doNoFrame: => A)(implicit turn: PipeliningTurn): A = {
    findFrame(doForFrame, doNoFrame)
  }

  protected[rescala] def forWriteFramesAfter(frame: CFrame)(op: CFrame => Unit): List[Frame[Content]] = lockPipeline {
    def collectFrames(head: CFrame = frame): List[Frame[Content]] = {
      if (head == null)
        List()
      else
        head :: collectFrames(head.next())
    }

    val frames = collectFrames()
    frames.foreach { op }
    frames
  }

  protected[rescala] def createFrame(implicit turn: PipeliningTurn): Unit = lockPipeline {
    assert(!hasFrame)
    def createFrame(prev: Content): CFrame = {
      val newFrame = Frame[Content](turn, this)
      newFrame.content = duplicate(prev, turn)
      assert(newFrame.turn == turn)
      newFrame
    }

    if (queueHead == null) {
      insertHead(createFrame(stableFrame.content))
    } else {
      val newFrame = createFrame(queueTail.content)
      insertAfter(newFrame, queueTail)
    }
    assert(hasFrame)
    assert(assertTurnOrder)
  }

  protected[rescala] def createFrame(frameInit: CFrame => Unit)(implicit turn: PipeliningTurn): Unit = lockPipeline {
    createFrame(turn)
    needFrame(frameInit(_))
  }

  protected[rescala] def createFrameBefore(implicit turn: PipeliningTurn): Unit = lockPipeline {
    assert(!hasFrame)
    def createFrame(prev: CFrame): CFrame = {
      val newFrame = Frame[Content](turn, this)
      
      // Usually one would copy the data from the previous one. 
      // When creating frames sequentially, this is true ever.
      // But for parallel framing we need to insert a frame above another one, while the other one
      // has changes in its outgoing due to an dynamic dependency add. Nothing else could have changed
      // before but it is necessary to take these changes into account. There will be never the case
      // that these changes should not be copied in this frame because the one which applied them to
      // all frames below it, is prev or an even earlier frame
      val copyFrame = if (prev.next() == null) prev else prev.next()
      
      newFrame.content = duplicate(copyFrame.content, turn)
      assert(newFrame.turn == turn)
      newFrame
    }

    def assertNoOtherFrameBefore(tail: CFrame): Boolean = {
      if (tail == null)
        true
      else if (turn < tail.turn)
        false
      else assertNoOtherFrameBefore(tail.previous())
    }

    def findPreviousFrame(tail: CFrame = queueTail): CFrame = {
      if (tail == null)
        null
      else if (turn < tail.turn)
        findPreviousFrame(tail.previous())
      else {
        assert(assertNoOtherFrameBefore(tail.previous()))
        tail
      }
    }

    val predecessor = findPreviousFrame()
    val newFrame = createFrame(predecessor)
    insertAfter(newFrame, predecessor)

    assert(hasFrame)
    assert(assertTurnOrder)
  }

  protected[rescala] def foreachFrameTopDown(action: CFrame => Unit): Unit = lockPipeline{
    @tailrec
    def impl(head: CFrame = queueHead): Unit = {
      if (head != null) {
        action(head)
        impl(head.next())
      }
    }
    impl()
  }

  protected[rescala] def insertWriteFrameFor(otherTurn: PipeliningTurn)(implicit turn: PipeliningTurn): Unit = {
    lockPipeline {
      assert(queueHead != null, s"At least the frame for $turn needs to be there")

      @tailrec
      def findFrameToInsertAfter(last: CFrame = queueTail): CFrame = {
        assert(last != null, s"No frame found, but at least frame for $turn should be there")
        val frameTurn = last.turn
        if (frameTurn == turn) {
          last
        } else if (otherTurn >= frameTurn) {
          assert(!last.isTouched && !last.isWritten)
          last
        } else {
          assert(!last.isTouched && !last.isWritten)
          findFrameToInsertAfter(last.previous())
        }
      }


      val preceedingFrame = findFrameToInsertAfter()

      val newFrame = Frame[Content](otherTurn, this)
      newFrame.content = duplicate(preceedingFrame.content, otherTurn)
      insertAfter(newFrame, preceedingFrame)
      assert(assertTurnOrder)
    }
  }

  protected[rescala] def deleteFrames(implicit turn: PipeliningTurn): Unit = lockPipeline {

    // Remove any frame and throw them away
    var currentFrame = queueTail

    while (currentFrame != null) {
      if (currentFrame.turn == turn) {
        val newFrame = currentFrame.previous()
        deleteFrame(currentFrame)
        currentFrame = newFrame
      } else {
        currentFrame = currentFrame.previous()
      }
    }

    // if there were multiple frames, they all need to be at head, so
    assert(!hasFrame, s" Frames for $turn left in ${getPipelineFrames()}")
  }

  protected[rescala] def removeFrames(implicit turn: PipeliningTurn): Unit = lockPipeline {
    // Assert for at least one frame
    assert(queueHead.turn == turn, s"Frame for $turn cannot be removed at $this because it is not head of the queue: ${getPipelineFrames()}")
    // Can remove the frame if it is head of the queue

    def isHeadOfThisTurn() = {
      if (queueHead == null)
        false
      else {
        queueHead.turn == turn
      }
    }

    while (isHeadOfThisTurn) {
      replaceStableFrame()
    }
    // if there were multiple frames, they all need to be at head, so
    assert(!hasFrame, s" Frames for $turn left in ${getPipelineFrames()}")
  }

  protected[rescala] def markWritten(implicit turn: PipeliningTurn): Unit = {
    needFrame(_.markWritten())
  }

  protected[rescala] def markTouched(implicit turn: PipeliningTurn): Unit = {
    needFrame(_.markTouched())
  }
}