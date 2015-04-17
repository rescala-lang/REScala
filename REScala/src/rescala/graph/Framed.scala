package rescala.graph

import rescala.turns.Turn
import scala.collection.immutable.Queue
import scala.annotation.tailrec

trait Framed {
  protected[this] type Frame <: TurnFrame;

  protected[this] def initialStableFrame: Frame
  protected[this] def newFrameFrom(turn: Turn, other: Frame): Frame
  protected[this] var stableFrame: Frame = initialStableFrame
  protected[this] var pipelineFrames: Queue[Frame] = Queue()
  
  private val pipelineLock = new Object
  
  private def lockPipeline[A](op:  => A) :A = pipelineLock.synchronized {
    op
  }
  
  // Access for testing
  protected[rescala] final def getPipelineFrames() = pipelineFrames

  // Does pipelining in this way remove STM support? If yes, is that a problem if 
  // we know now, that it is not that useful?

  // TODO add synchronization for thread safe access to queue

  // Locking phase as usual, but reactives can be locked if current frame is written
  //   In the locking phase, after all locks could be gained, insert new frames for each
  //   involved reactive => other turns cannot lock them until the frames were set to be written
  //   

  private def findFrame[T](find: Option[Frame] => T)(implicit turn: Turn): T = lockPipeline {
    val selectedFrame = pipelineFrames.find { x => x.turn eq turn }
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
    def findTopMostFrame(queue : Queue[Frame]) : Frame = queue match{
      case Queue() => stableFrame
      case _ :+ last if turn.waitsOnFrame(last.turn) => last
      case begin :+ _ => findTopMostFrame(begin)
    }
    val topMostWaitingFrame = findTopMostFrame(pipelineFrames)
    f(topMostWaitingFrame)
  }
  
  protected [rescala] def isPreviousFrameFinished(implicit turn : Turn) : Boolean = lockPipeline {
    @tailrec
    def indexOf(queue : Queue[Frame], index : Int) : Option[Int] = queue match {
      case Queue() => None
      case head +: _ if head.turn == turn =>  Some(index)
      case _ +: tail => indexOf(tail, index +1)
    }
    val frameIndexOption = indexOf(pipelineFrames, 0)
    assert (frameIndexOption.isDefined, "No frame for turn " + turn + " found")
    frameIndexOption.get match {
      case 0 => true // Stable frame is always finished
      case frameIndex => pipelineFrames(frameIndex -1).isWritten()
    }
  }

  protected[rescala] def hasFrame(implicit turn: Turn): Boolean = {
    findFrame(_ => true, false)
  }

  protected[rescala] def createFrame(allowedAfterFrame: Frame => Boolean = { x: Frame => true })(implicit turn: Turn): Unit = lockPipeline {
    def createFrame(lastElem : Frame) : Frame =  {
      val newFrame = newFrameFrom(turn, lastElem)
      assert(newFrame.turn == turn)
      newFrame
    }
    def insertFrame(queue: Queue[Frame], lastElem: Frame): Queue[Frame] = queue match {
      case Queue() => Queue(createFrame(lastElem))
      case head +: tail if allowedAfterFrame(head) => head +: insertFrame(tail, head)
      case _ => createFrame(lastElem) +: queue
    }
    pipelineFrames = insertFrame(pipelineFrames, stableFrame)
    assert(hasFrame)
  }
  
  protected[rescala] def fillFrame(implicit turn : Turn) : Unit = lockPipeline {
    def fill (queue:Queue[Frame] ) : Queue[Frame] = queue match{ 
      case Queue() :+ last if last.turn == turn => Queue(last)
      case tail :+ last if last.turn == turn => tail :+ newFrameFrom(turn, tail.last)
      case tail :+ last => fill(tail) :+ last
      case Queue() :+ last if last.turn == null => throw new AssertionError(s"No frame found for turn $turn")
    }
    assert(pipelineFrames.nonEmpty, "No frame there")
    pipelineFrames = fill(pipelineFrames)
  }

  protected[rescala] def moveFrameBack(allowedAfterFrame: Frame => Boolean)(implicit turn: Turn): Unit = lockPipeline {
    def moveFrame(queue: Queue[Frame], frame: Frame): Queue[Frame] = queue match {
      case Queue() if frame != null => Queue(frame)
      case head +: tail if head.turn == turn => moveFrame(tail, head)
      case head +: tail if frame != null && allowedAfterFrame(head) => head +: frame +: tail
      case head +: tail => head +: moveFrame(tail, frame)
      case Queue() if frame == null => throw new AssertionError(s"No frame found for turn $turn")
    }
    pipelineFrames = moveFrame(pipelineFrames, null.asInstanceOf[Frame])
  }

  protected[rescala] def tryRemoveFrame(implicit turn: Turn): Unit = lockPipeline {
    // Can remote the frame if it is head of the queue
    if (pipelineFrames.head.turn == turn) {
      def removeFrames(stable : Frame, queue : Queue[Frame]) : (Frame, Queue[Frame]) = queue match{
        case head +: tail if head.shouldBeRemoved() => head.removeTurn(); removeFrames(head, tail)
        case _ => (stable, queue)
      }
      pipelineFrames.head.removeTurn()
      val (newStableFrame, newQueue) = removeFrames(pipelineFrames.head, pipelineFrames.tail)
      stableFrame = newStableFrame
      pipelineFrames = newQueue
    } else {
    //  println(s"Mark remove $turn at $this")
      frame().markToBeRemoved()
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