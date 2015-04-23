package rescala.graph

import rescala.turns.Turn
import scala.collection.immutable.Queue
import scala.annotation.tailrec

trait Framed {
  protected[this] type Frame <: TurnFrame[Frame];

  protected[this] def initialStableFrame: Frame
  protected[this] def newFrameFrom(turn: Turn, other: Frame): Frame
  protected[this] var stableFrame: Frame = initialStableFrame
 // protected[this] var pipelineFrames: Queue[Frame] = Queue()
  protected[this] var queueHead : Frame = null.asInstanceOf[Frame];
  protected[this] var queueTail : Frame = null.asInstanceOf[Frame];
  
  private object pipelineLock
  
  private def lockPipeline[A](op:  => A) :A = pipelineLock.synchronized {
    op
  }
  
  // Access for testing
  protected[rescala] final def getPipelineFrames() = lockPipeline {
    def makeQueue(head : Frame, queue : Queue[Frame]) : Queue[Frame] = {
      if (head == null)
        queue
      else
        makeQueue(head.next(), queue :+ head)
    }
    makeQueue(queueHead, Queue())
  }

  // Does pipelining in this way remove STM support? If yes, is that a problem if 
  // we know now, that it is not that useful?

  // TODO add synchronization for thread safe access to queue

  // Locking phase as usual, but reactives can be locked if current frame is written
  //   In the locking phase, after all locks could be gained, insert new frames for each
  //   involved reactive => other turns cannot lock them until the frames were set to be written
  //   

  private def findFrame[T](find: Option[Frame] => T)(implicit turn: Turn): T = lockPipeline {
    @tailrec
    def findFrame(head : Frame) : Option[Frame]= {
      if (head == null)
        None
      else if (head.turn eq turn)
        Some(head)
      else findFrame(head.next)
    }
    val selectedFrame = findFrame(queueHead)
    find(selectedFrame)
  }

  private def findFrame[T](found: Frame => T, notFound: => T)(implicit turn: Turn): T = {
    findFrame(_ match {
      case Some(d) => found(d)
      case None    => notFound
    })
  }

  protected def frame[T](f: Frame => T = { x: Frame => x })(implicit turn: Turn): T = lockPipeline {
    @tailrec
    def findTopMostFrame(tail : Frame) : Frame = {
      if (tail == null)
        stableFrame
      else if (turn.waitsOnFrame(tail.turn))
        tail
      else
        findTopMostFrame(tail.previous())
    }
    val topMostWaitingFrame = findTopMostFrame(queueTail)
    
    f(topMostWaitingFrame)
  }
  
  protected[rescala] def waitUntilCanWrite(implicit turn : Turn) : Unit = {
    val turnFrame = frame()
    turnFrame.awaitPredecessor(pipelineLock)
  }

  protected[rescala] def hasFrame(implicit turn: Turn): Boolean = {
    findFrame(_ => true, false)
  }

  protected[rescala] def createFrame(visitPreviousFrame: Frame => Unit = { x: Frame => })(implicit turn: Turn): Unit = lockPipeline {
    def createFrame(lastElem : Frame) : Frame =  {
      val newFrame = newFrameFrom(turn, lastElem)
      assert(newFrame.turn == turn)
      newFrame
    }
    
    @tailrec
    def visitPipeline(head : Frame) : Unit = {
      if (head != null) {
        visitPreviousFrame(head)
        visitPipeline(head.next)
      }
    }
    
    if (queueHead == null) {
      queueHead = createFrame(stableFrame)
      queueTail = queueHead
    } else {
      visitPipeline(queueHead)
      val newFrame = createFrame(queueTail)
      newFrame.insertAfter(queueTail)
      queueTail = newFrame
    }
    assert(hasFrame)
  }
  
  protected[rescala] def fillFrame(implicit turn : Turn) : Unit = lockPipeline {
    
    @tailrec
    def refreshFrame(head : Frame) : Unit = {
      if (head == null) {
        throw new AssertionError(s"No frame found for turn $turn")
      } else if (head.turn == turn) {
        if (head.previous() != null) {
          val newFrame = newFrameFrom(turn, head.previous())
          if (head.next() == null) {
            queueTail = newFrame
          }         
          head.replaceWith(newFrame)
        } else {
          val newFrame = newFrameFrom(turn, stableFrame)
          queueHead = newFrame
          if (head.next() == null) {
            queueTail = newFrame
          }
          head.replaceWith(newFrame)
        }
      } else {
        refreshFrame(head.next())
      }
    } 
    refreshFrame(queueHead)
  }

  protected[rescala] def moveFrameBack(allowedAfterFrame: Frame => Boolean)(implicit turn: Turn): Unit = lockPipeline {
    
    def moveFrame(head : Frame, frame : Frame) : (Frame, Frame) = {
      if (head == null) {
        throw new AssertionError("Frame not allowed after any frame in the pipeline")
      } else if (head.turn eq turn) {
        moveFrame(head.next(), head)
      } else if (frame == null) {
        // Did not find the frame for the turn
        moveFrame(head.next(), null.asInstanceOf[Frame])
      } else if (allowedAfterFrame(head)) {
        val newHead = if (frame.previous() == null) frame.next else queueHead
        val newTail = if(head.next() == null) frame else queueTail
        frame.moveAfter(head)
        (newHead, newTail)
      } else {
        moveFrame(head.next(), frame)
      }
    }
    
    val (newHead, newTail) = moveFrame(queueHead, null.asInstanceOf[Frame])
    queueHead = newHead
    queueTail = newTail
  }

  protected[rescala] def removeFrame(implicit turn: Turn): Unit = lockPipeline {
    // Can remote the frame if it is head of the queue
    if (queueHead.turn == turn) {
      val newHead = queueHead.next()
      val newTail = if (queueTail == queueHead) null.asInstanceOf[Frame] else queueTail
      queueHead.removeFrame()
      queueHead.removeTurn()
      stableFrame = queueHead
      queueHead = newHead
      queueTail = newTail
    } else {
    //  println(s"Mark remove $turn at $this")
      assert(false, s"Frame for $turn cannot be removed at $this because it is not head of the queue")
    }
  }

  protected[rescala] def markWritten(implicit turn: Turn): Unit = {
    frame().markWritten()
  }

  // If want to omit the buffers in the turn data (because the previous data is contained
  // in the frame before), I can only remove the head turn in the queue and need to remove
  // other turns only if they get head in the queue
  // Then I need to store in the Turn whether it is completed

}