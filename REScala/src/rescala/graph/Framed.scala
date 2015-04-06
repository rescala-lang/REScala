package rescala.graph

import rescala.turns.Turn
import scala.collection.immutable.Queue

trait Framed {
  protected[this]type D <: ReactiveTurnData;

  protected def initialStableFrame: D;
  protected def newFrameFrom(turn: Turn, other: D): D;
  private[rescala] var stableFrame: D = initialStableFrame
  private[rescala] var pipelineFrames: Queue[D] = Queue()

  // Does pipelining in this way remove STM support? If yes, is that a problem if 
  // we know now, that it is not that useful?

  // TODO add synchronization for thread safe access to queue

  // Locking phase as usual, but reactives can be locked if current frame is written
  //   In the locking phase, after all locks could be gained, insert new frames for each
  //   involved reactive => other turns cannot lock them until the frames were set to be written
  //   
  /*
  protected [rescala] def moveFrame(currentTurn : Turn, before : Turn) : Unit = {
    val currentFrame = frame()(currentTurn)
    @tailrec def moveFrameImpl(queue: Queue[D]) : Queue[D] = queue match{
      case head :+ rest =>
        if (head == before)
          currentFrame :+ (head :+ moveFrameImpl(rest))
        else if (head == currentFrame)
          moveFrameImpl(rest)
        else
          head :+ moveFrameImpl(rest)
      case Queue() => Queue()
    }
  }*/

  private def findFrame[T](find: Option[D] => T)(implicit turn: Turn): T = {
    val selectedFrame = pipelineFrames.find { x => x.turn.get eq turn }
    find(selectedFrame)
  }

  private def findFrame[T](found: D => T, notFound: => T)(implicit turn: Turn): T = {
    findFrame(_ match {
      case Some(d) => found(d)
      case None    => notFound
    })
  }

  protected def frame[T](f: D => T = { x: D => x })(implicit turn: Turn): T = {
    // f(stableFrame) is short fix for tests
    // an assertion error should be there
    findFrame(f(_), f(stableFrame))
  }

  protected[rescala] def hasFrame(implicit turn: Turn): Boolean = {
    findFrame(_ => true, false)
  }

  protected[rescala] def createFrame (allowedAfterFrame: D => Boolean = { x: D => true }) (implicit turn: Turn) : Unit = {
    def insertFrame(queue: Queue[D], lastElem: D): Queue[D] = {
      if (queue.isEmpty) {
        Queue(newFrameFrom(turn, lastElem))
      } else {
        val head = queue.head
        val tail = queue.tail
        if (allowedAfterFrame(head)) {
          insertFrame(tail, head) :+ head
        } else {
          queue :+ newFrameFrom(turn, lastElem)
        }
      }
    }
    insertFrame(pipelineFrames, stableFrame)
  }

  protected[rescala] def tryRemoveFrame(implicit turn: Turn): Unit = {
    // Can remote the frame if it is head of the queue
    if (pipelineFrames.head.turn.get == turn) {
      var newStable = pipelineFrames.head
      pipelineFrames = pipelineFrames.tail;
      // Remove all next frames, which are marked to be removed
      while (pipelineFrames.head.shouldBeRemoved()) {
        newStable = pipelineFrames.head
        pipelineFrames = pipelineFrames.tail
      }
      stableFrame = newStable
    } else {
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

  private def finishTurn(turn: Turn): Unit = {
    def removeTurn(queue: Queue[D]): Queue[D] = {
      if (queue.isEmpty) {
        queue
      } else {
        val head = queue.head
        if (head.turn.get eq turn)
          queue
        else if (!head.isWritten())
          throw new AssertionError("A turn could not be added if any preceeding has not written")
        else
          head +: removeTurn(queue)
      }
    }
    pipelineFrames = removeTurn(pipelineFrames)
  }

}