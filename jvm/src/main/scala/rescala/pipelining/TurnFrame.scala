package rescala.pipelining

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.locks.LockSupport
import rescala.util.JavaFunctionsImplicits._

object Frame {

  protected[rescala] final def awaitUntilWritten(modificationLock: AnyRef, at: Pipeline, waitingTurn: PipeliningTurn): Unit = {

    var wait = true
    var needCleanCheck = true
    while (wait || needCleanCheck) {
      at.getStableFrame().waitForWritten(modificationLock, waitingTurn) match {
        case Some(waitOnFrame) =>
          assert(waitingTurn > waitOnFrame.turn)
          LockSupport.park(waitOnFrame.creatorThread)
          wait = true
        case None =>
          if (!wait)
            needCleanCheck = false
          wait = false
      }
    }
    assert({
      at.foreachFrameTopDown { frame =>
        if (frame.turn != null && waitingTurn > frame.turn)
          assert(frame.isWritten, s"Did not wait properly on other turns for turn $waitingTurn at frame $frame at ${at.getPipelineFramesWithStable()}  ")
      }
      true
    })
  }

}

case class Frame[T](var turn: PipeliningTurn, val at: Pipeline) {

  private var predecessor: Frame[T] = null.asInstanceOf[Frame[T]]
  private var successor: Frame[T] = null.asInstanceOf[Frame[T]]
  protected[pipelining] final def next() = successor
  protected[pipelining] final def previous() = predecessor

  private val creatorThread = Thread.currentThread()
  protected val lockObject = new Object
  private var lockedOnThread: Set[Thread] = Set()

  var oldTurn: PipeliningTurn = null

  protected[pipelining] var content: T = null.asInstanceOf[T];

  private val touched = new AtomicBoolean(false); // I think do not need atomic here, because it is should only be accessed by one thread
  private val written = new AtomicBoolean(false);

  protected[pipelining] def isTouched: Boolean = touched.get
  protected[pipelining] def markTouched(): Unit = touched.set(true)

  protected[pipelining] def isWritten = written.get

  protected[pipelining] final def markWritten() = {
    assert(!written.get)
    lockObject.synchronized {
      // Only retry threads if was not marked written already
      written.set(true)
      retryBlockedThreads()
    }
  }

//  override def toString() = s"${getClass.getSimpleName}(turn=$turn, " + (if (oldTurn != null) s"oldTurn=$oldTurn, " else "") + s"written=${isWritten}, touches=${isTouched})[$content]"

  protected def retryBlockedThreads() = lockObject.synchronized {
    val blockedThreads = lockedOnThread
    lockedOnThread = Set()
    blockedThreads.foreach { LockSupport.unpark(_) }
  }


  private def waitForWritten(modificationLock: AnyRef, waitingTurn: PipeliningTurn): Option[Frame[T]] = {
    modificationLock.synchronized {
      val foundFrame = if (successor != null && waitingTurn > successor.turn)
        successor.waitForWritten(modificationLock, waitingTurn)
      else None
      if (foundFrame.isDefined)
        foundFrame
      else lockObject.synchronized {
        if (waitingTurn == turn || isWritten)
          None
        else {
          lockedOnThread += Thread.currentThread()
          Some(this)
        }
      }

    }
  }

  protected[pipelining] final def removeFrame() = {
    if (successor != null) {
      successor.predecessor = this.predecessor
    }
    if (predecessor != null) {
      predecessor.successor = this.successor
    }
    retryBlockedThreads()
    val oldSuccessor = this.successor
    this.successor = null.asInstanceOf[Frame[T]]
    this.predecessor = null.asInstanceOf[Frame[T]]

    if (oldSuccessor != null) {
      oldSuccessor.retryBlockedThreads()
    }
  }

  protected[pipelining] final def moveAfter(newPredecessor: Frame[T]) = {
    assert(newPredecessor != null)
    removeFrame()
    insertAfter(newPredecessor)
  }

  protected[pipelining] final def insertAfter(newPredecessor: Frame[T]) = {
    assert(predecessor == null)

    val newSuccessor = newPredecessor.successor
    if (newSuccessor != null) {
      newSuccessor.predecessor = this
      successor = newSuccessor
    }
    newPredecessor.successor = this
    predecessor = newPredecessor

    retryBlockedThreads()
    if (newSuccessor != null) {
      newSuccessor.retryBlockedThreads()
    }
  }

}
