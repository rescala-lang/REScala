package rescala.graph

import rescala.turns.Turn
import scala.collection.immutable.Queue
import scala.annotation.tailrec

trait Framed {
  protected[this] type Content ;

  private type CFrame = Frame[Content]
  
  protected[this] def initialStableFrame: Content
  protected[this] def duplicate(content : Content) : Content
  protected[this] var stableFrame: Content = initialStableFrame
 
  protected[this] var queueHead : CFrame = null.asInstanceOf[CFrame];
  protected[this] var queueTail : CFrame = null.asInstanceOf[CFrame];
  
  private object pipelineLock
  
  private def lockPipeline[A](op:  => A) :A = pipelineLock.synchronized {
    op
  }
  
  // Access for testing
  protected[rescala] final def getPipelineFrames() = lockPipeline {
    def makeQueue(head : CFrame, queue : Queue[CFrame]) : Queue[CFrame] = {
      if (head == null)
        queue
      else
        makeQueue(head.next(), queue :+ head)
    }
    makeQueue(queueHead, Queue())
  }
 

  protected[rescala] def findFrame[T](find: Option[CFrame] => T)(implicit turn: Turn): T = lockPipeline {
    @tailrec
    def findFrame(head : CFrame) : Option[CFrame]= {
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
  
  protected[rescala] def needFrame[T](op : CFrame => T = {x:CFrame=>x})(implicit turn : Turn) : T = {
    findFrame(_ match {
      case Some(d) => op(d)
      case None => throw new AssertionError(s"No frame found for $turn at $this")
    })
  }
  
   protected def frame[T](f: Content => T = { x: Content => x })(implicit turn: Turn): T = {
    @tailrec
    def findBottomMostFrame(tail : CFrame) : Content = {
      if (tail == null)
        stableFrame
      else if (turn.waitsOnFrame(tail.turn))
        tail.content
      else
        findBottomMostFrame(tail.previous())
    }
    
    // Local: if the turn itself is found, it is the bottom most frame => no need to sync
    val bottomMostWaitingFrame : Content = findFrame(x => x) match {
      case Some(frame) => frame.content
      case None => turn.waitsOnLock { lockPipeline{
        findBottomMostFrame(queueTail)
      }}
    }
    
    f(bottomMostWaitingFrame)
  }
  
  protected[rescala] def waitUntilCanWrite(implicit turn : Turn) : Unit = {
    findFrame(x => x) match {
      case Some(turnFrame) => turnFrame.awaitPredecessor(pipelineLock)
      case None => throw new AssertionError(s"No frame for $turn at $this")
    }
    
  }

  protected[rescala] def hasFrame(implicit turn: Turn): Boolean = {
    findFrame(_ => true, false)
  }

  protected[rescala] def createFrame(visitPreviousFrame: CFrame => Unit = { x: CFrame => })(implicit turn: Turn): Unit = lockPipeline {
    def createFrame(prev : Content) : CFrame =  {
      val newFrame = WriteFrame[Content](turn)
      newFrame.content = duplicate(prev)
      assert(newFrame.turn == turn)
      newFrame
    }
    
    @tailrec
    def visitPipeline(head : CFrame) : Unit = {
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
      val newFrame = createFrame(queueTail.content)
      newFrame.insertAfter(queueTail)
      queueTail = newFrame
    }
    assert(hasFrame)
  }
  
  protected[rescala] def fillFrame(implicit turn : Turn) : Unit = lockPipeline {
    
    @tailrec
    def refreshFrame(head : CFrame) : Unit = {
      if (head == null) {
        throw new AssertionError(s"No frame found for turn $turn")
      } else if (head.turn == turn) {
        val newContent = duplicate (if (head.previous() == null) stableFrame else head.previous().content)
        head.content = newContent
      } else {
        refreshFrame(head.next())
      }
    } 
    refreshFrame(queueHead)
  }

  protected[rescala] def moveFrameBack(allowedAfterFrame: CFrame => Boolean)(implicit turn: Turn): Unit = lockPipeline {
    
    def moveFrame(head : CFrame, frame : CFrame) : (CFrame, CFrame) = {
      if (head == null) {
        throw new AssertionError("Frame not allowed after any frame in the pipeline")
      } else if (head.turn eq turn) {
        moveFrame(head.next(), head)
      } else if (frame == null) {
        // Did not find the frame for the turn
        moveFrame(head.next(), null.asInstanceOf[CFrame])
      } else if (allowedAfterFrame(head)) {
        val newHead = if (frame.previous() == null) frame.next else queueHead
        val newTail = if(head.next() == null) frame else queueTail
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
      val newTail = if (queueTail == queueHead) null.asInstanceOf[CFrame] else queueTail
      queueHead.removeFrame()
      stableFrame = queueHead.content
      queueHead = newHead
      queueTail = newTail
    } else {
    //  println(s"Mark remove $turn at $this")
      assert(false, s"Frame for $turn cannot be removed at $this because it is not head of the queue")
    }
  }

  protected[rescala] def markWritten(implicit turn: Turn): Unit = {
    needFrame(_.asInstanceOf[WriteFrame[Content]].markWritten())
  }
  
  protected[rescala] def markTouched(implicit turn : Turn) : Unit = {
    needFrame(_.markTouched())
  }

  // If want to omit the buffers in the turn data (because the previous data is contained
  // in the frame before), I can only remove the head turn in the queue and need to remove
  // other turns only if they get head in the queue
  // Then I need to store in the Turn whether it is completed

}