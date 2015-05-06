package rescala.graph

import rescala.turns.Turn
import scala.collection.immutable.Queue
import scala.annotation.tailrec

trait Framed {
  // self : Reactive =>

  protected[this]type Content;

  private type CFrame = Frame[Content]

  protected[this] def initialStableFrame: Content
  protected[this] def duplicate(content: Content): Content
  protected[this] var stableFrame: Content = initialStableFrame

  protected[this] var queueHead: CFrame = null.asInstanceOf[CFrame];
  protected[this] var queueTail: CFrame = null.asInstanceOf[CFrame];

  protected[this] var incompleteDynamicFrames: Map[Framed, List[Frame[_]]] = Map()
  protected[rescala] def getIncompleteDynamicFrames = incompleteDynamicFrames

  private object pipelineLock

  private def lockPipeline[A](op: => A): A = pipelineLock.synchronized {
    op
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

  protected[rescala] def findFrame[T](find: Option[CFrame] => T)(implicit turn: Turn): T = lockPipeline {
    @tailrec
    def findFrame(head: CFrame): Option[CFrame] = {
      if (head == null)
        None
      else if (head.turn eq turn)
        Some(head)
      else findFrame(head.next)
    }
    val selectedFrame = findFrame(queueHead)
    find(selectedFrame)
  }

  private def findFrame[T](found: CFrame => T, notFound: => T)(implicit turn: Turn): T = {
    findFrame(_ match {
      case Some(d) => found(d)
      case None    => notFound
    })
  }

  protected[rescala] def needFrame[T](op: CFrame => T = { x: CFrame => x })(implicit turn: Turn): T = {
    findFrame(_ match {
      case Some(d) => op(d)
      case None    => throw new AssertionError(s"No frame found for $turn at $this")
    })
  }

  private def frame(implicit turn: Turn): Option[CFrame] = {
    @tailrec
    def findBottomMostFrame(tail: CFrame): Option[CFrame] = {
      if (tail == null)
        None
      else if (turn >>~ tail.turn)
        Some(tail)
      else
        findBottomMostFrame(tail.previous())
    }

    // Are not allowed to hold the pipelineLock, because than we cannot
    // query for the graph lock, to prohibit deadlocks
    assert(!Thread.holdsLock(pipelineLock))

    // Local: if the turn itself is found, it is the bottom most frame => no need to sync
    val bottomMostWaitingFrame: Option[CFrame] = findFrame(x => x).orElse({
      turn.waitsOnLock {
        lockPipeline {
          findBottomMostFrame(queueTail)
        }
      }
    })
    bottomMostWaitingFrame
  }

  protected def frame[T](f: Content => T = { x: Content => x })(implicit turn: Turn): T = {
    val content = frame.map(_.content).getOrElse(stableFrame)
    f(content)
  }

  protected[rescala] def waitUntilCanWrite(implicit turn: Turn): Unit = {
    findFrame(x => x) match {
      case Some(turnFrame) => turnFrame.awaitPredecessor(pipelineLock)
      case None            => throw new AssertionError(s"No frame for $turn at $this")
    }
  }

  protected[rescala] def waitUntilCanRead(implicit turn: Turn): Unit = {
    // TODO IF keep frame reordering, need to do something more here. because the frame
    // we need to read from may change
    frame match {
      case Some(frame) => frame match {
        case WriteFrame(_, _) =>
        case _                => frame.awaitUntilWritten()
      }
      case None =>
    }
  }

  protected[rescala] def hasFrame(implicit turn: Turn): Boolean = {
    findFrame(_ => true, false)
  }

  protected[rescala] def writeFramesAfter(frame: CFrame): List[WriteFrame[Content]] = lockPipeline {
    def collectFrames(head: CFrame = frame): List[WriteFrame[Content]] = {
      if (head == null)
        List()
      else
        head match {
          case (writeFrame @ WriteFrame(_, _)) => writeFrame :: collectFrames(head.next())
          case _                               => collectFrames(head.next())
        }
    }

    collectFrames()
  }

  protected[rescala] def createFrame(implicit turn: Turn): Unit = lockPipeline {
    assert(!hasFrame)
    def createFrame(prev: Content): CFrame = {
      val newFrame = WriteFrame[Content](turn, this)
      newFrame.content = duplicate(prev)
      assert(newFrame.turn == turn)
      newFrame
    }

    if (queueHead == null) {
      queueHead = createFrame(stableFrame)
      queueTail = queueHead
    } else {
      val newFrame = createFrame(queueTail.content)
      newFrame.insertAfter(queueTail)
      queueTail = newFrame
    }
    assert(hasFrame)
  }

  protected[rescala] def createFrameBefore(stillBefore: Turn => Boolean)(implicit turn: Turn): Unit = lockPipeline {
    assert(!hasFrame)
    def createFrame(prev: Content): CFrame = {
      val newFrame = WriteFrame[Content](turn, this)
      newFrame.content = duplicate(prev)
      assert(newFrame.turn == turn)
      newFrame
    }

    def findPreviousFrame(tail: CFrame = queueTail): CFrame = {
      if (tail == null)
        null
      else if (stillBefore(tail.turn))
        findPreviousFrame(tail.previous())
      else
        tail
    }

    if (queueTail == null) {
      queueHead = createFrame(stableFrame)
      queueTail = queueHead
    } else {
      val insertAfter = findPreviousFrame()
      if (insertAfter == null) {
        val newFrame = createFrame(stableFrame)
        queueHead.insertAfter(newFrame)
        queueHead = newFrame
      } else {
        val newFrame = createFrame(insertAfter.content)
        newFrame.insertAfter(insertAfter)
        if (queueTail == insertAfter) {
          queueTail = newFrame
        }
      }
    }
    assert(hasFrame)
  }

  protected[rescala] def foreachFrameTopDown(action: CFrame => Unit): Unit = {
    @tailrec
    def impl(head: CFrame = queueHead): Unit = {
      if (head != null) {
        action(head)
        impl(head.next())
      }
    }
    impl()
  }

  protected[rescala] def insertWriteFrameFor(otherTurn: Turn)(implicit turn: Turn): Unit = turn.waitsOnLock {
    lockPipeline {
      assert(queueHead != null, s"At least the frame for $turn needs to be there")

      @tailrec
      def findFrameToInsertAfter(last: CFrame = queueTail): CFrame = {
        assert(last != null, s"No frame found, but at least frame for $turn should be there")
        val frameTurn = last.turn
        if (frameTurn == turn) {
          last
        } else if (otherTurn >>~ frameTurn) {
          last
        } else {
          findFrameToInsertAfter(last.previous())
        }
      }

      val preceedingFrame = findFrameToInsertAfter()

      val newFrame = WriteFrame[Content](otherTurn, this)
      newFrame.content = duplicate(preceedingFrame.content)
      newFrame.insertAfter(preceedingFrame)
    }
  }

  protected[rescala] def fillFrame(implicit turn: Turn): Unit = lockPipeline {

    @tailrec
    def refreshFrame(head: CFrame): Unit = {
      if (head == null) {
        throw new AssertionError(s"No frame found for turn $turn")
      } else if (head.turn == turn) {
        val newContent = duplicate(if (head.previous() == null) stableFrame else head.previous().content)
        head.content = newContent
      } else {
        refreshFrame(head.next())
      }
    }
    refreshFrame(queueHead)
  }

  protected[rescala] def moveFrameBack(allowedAfterFrame: CFrame => Boolean)(implicit turn: Turn): Unit = lockPipeline {

    def moveFrame(head: CFrame, frame: CFrame): (CFrame, CFrame) = {
      if (head == null) {
        throw new AssertionError("Frame not allowed after any frame in the pipeline")
      } else if (head.turn eq turn) {
        moveFrame(head.next(), head)
      } else if (frame == null) {
        // Did not find the frame for the turn
        moveFrame(head.next(), null.asInstanceOf[CFrame])
      } else if (allowedAfterFrame(head)) {
        val newHead = if (frame.previous() == null) frame.next else queueHead
        val newTail = if (head.next() == null) frame else queueTail
        frame.moveAfter(head)
        (newHead, newTail)
      } else {
        moveFrame(head.next(), frame)
      }
    }

    val (newHead, newTail) = moveFrame(queueHead, null.asInstanceOf[CFrame])
    queueHead = newHead
    queueTail = newTail
  }

  protected[rescala] def removeFrame(implicit turn: Turn): Unit = lockPipeline {
    // Can remote the frame if it is head of the queue
    if (queueHead.turn == turn) {
      val newHead = queueHead.next()
      val newTail = if (newHead == null) null.asInstanceOf[CFrame] else queueTail
      queueHead.removeFrame()
      stableFrame = queueHead.content
      queueHead = newHead
      queueTail = newTail
    } else {
      //  println(s"Mark remove $turn at $this")
      assert(false, s"Frame for $turn cannot be removed at $this because it is not head of the queue: ${getPipelineFrames()}")
    }
  }

  protected[rescala] def markWritten(implicit turn: Turn): Unit = {
    needFrame(_.markWritten())
  }

  protected[rescala] def markTouched(implicit turn: Turn): Unit = {
    needFrame(_.markTouched())
  }

  protected[rescala] def createDynamicFrame[T <: CFrame](makeFrame: => T)(from: Reactive)(implicit turn: Turn): T = {
    assert(!hasFrame)
    val predeceedingFrameOpt: Option[CFrame] = frame
    lockPipeline {
      val readFrame = makeFrame
      predeceedingFrameOpt match {
        case Some(predecessor) =>
          readFrame.content = duplicate(predecessor.content)
          val insertAtEnd = predecessor == queueTail
          readFrame.insertAfter(predecessor)
          if (insertAtEnd)
            queueTail = readFrame
        case None =>
          readFrame.content = duplicate(stableFrame)
          if (queueHead == null) {
            queueHead = readFrame
            queueTail = queueHead
          } else {
            queueHead.insertAfter(readFrame)
            queueHead = readFrame
          }
      }
      readFrame
    }
  }

  protected[rescala] def createDynamicReadFrame(from: Reactive)(implicit turn: Turn): DynamicReadFrame[Content] = {
    createDynamicFrame(DynamicReadFrame[Content](turn, this, from))(from)
  }

  protected[rescala] def createDynamicDropFrame(from: Reactive)(implicit turn: Turn): DynamicDropFrame[Content] = {
    createDynamicFrame(DynamicDropFrame[Content](turn, this, from))(from)
  }

  protected[rescala] def registerDynamicFrame(frame: DynamicReadFrame[_ <: ReactiveFrame]) = {
    internalRegisterDynamicFrame(frame)
  }

  protected[rescala] def registerDynamicFrame(frame: DynamicDropFrame[_ <: ReactiveFrame]) = {
    internalRegisterDynamicFrame(frame)
  }

  protected[rescala] def forgetDynamicFramesUntil(frame: Frame[_]) = {
    val reactive = frame.at
    val frames = incompleteDynamicFrames(frame.at)
    val newFrames = frames.dropWhile { _ != frame }.drop(1)
    if (newFrames.isEmpty)
      incompleteDynamicFrames -= reactive
    else
      incompleteDynamicFrames += (reactive -> newFrames)
  }

  private def internalRegisterDynamicFrame(frame: Frame[_]) = {
    val reactive = frame.at
    val newFramesForReactive = incompleteDynamicFrames.getOrElse(reactive, List()) :+ frame
    incompleteDynamicFrames += (reactive -> newFramesForReactive)
  }

  // If want to omit the buffers in the turn data (because the previous data is contained
  // in the frame before), I can only remove the head turn in the queue and need to remove
  // other turns only if they get head in the queue
  // Then I need to store in the Turn whether it is completed

}