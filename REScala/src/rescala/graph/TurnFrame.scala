package rescala.graph

import rescala.synchronization.{ TurnLock }
import rescala.turns.Turn
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.locks.Condition
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.locks.LockSupport
import java.util.concurrent.atomic.AtomicReference
import rescala.util.JavaFunctionsImplicits._
import rescala.pipelining.PipelineBuffer

sealed abstract class Frame[T](val turn: Turn, val at: PipelineBuffer) {

  private var predecessor: Frame[T] = null.asInstanceOf[Frame[T]]
  private var successor: Frame[T] = null.asInstanceOf[Frame[T]]
  protected[rescala] final def next() = successor
  protected[rescala] final def previous() = predecessor

  private val creatorThread = Thread.currentThread()
  protected val lockObject = new Object
  private var lockedOnThread: Set[Thread] = Set()

  protected[rescala] var content: T = null.asInstanceOf[T];

  private val touched = new AtomicBoolean(false); // I think do not need atomic here, because it is should only be accessed by one thread
  private val written = new AtomicBoolean(false);
  private val suspicious = new AtomicBoolean(false); // Is accessed by multiple threads, but should be in a safe way, may remove atomis here, too

  protected[rescala] def isTouched: Boolean = touched.get
  protected[rescala] def markTouched(): Unit = touched.set(true)
  
  protected[rescala] def isSuspicious() : Boolean = suspicious.get
  protected[rescala] def markSuspicious() : Unit = suspicious.set(true)

  protected[rescala] def isWritten = written.get

  protected[rescala] final def markWritten() = {
    if (!written.get)
      lockObject.synchronized {
        // Only retry threads if was not marked written already
        written.set(true)
        retryBlockedThreads()
      }
  }

  override def toString() = s"${getClass.getSimpleName}($turn, written=${isWritten})[$content]"

  protected def retryBlockedThreads() = lockObject.synchronized {
    val blockedThreads = lockedOnThread
    lockedOnThread = Set()
    blockedThreads.foreach { LockSupport.unpark(_) }
  }

  /**
   * Waits until the predecessor of this frame has written. The predecessor
   * of the frame may change but only on code which is synchronized on the given
   * modificationLock object.
   */
  protected[rescala] final def awaitPredecessor(modificationLock: AnyRef) = {
    var waits = true
    while (waits) {
      // Get the predecessor and check whether we need to wait on it
      // This is synchronized on the modification lock because the predecessor
      // is not allowed to be changed by an other thread of this block
      // is executed
      val creatorThread = modificationLock.synchronized {
        if (predecessor != null) {
          predecessor.lockObject.synchronized {
            // Register the current thread as waiting if need to wait
            waits = !predecessor.isWritten
            if (waits)
              predecessor.lockedOnThread += Thread.currentThread()
          }
          predecessor.creatorThread
        } else {
          waits = false
          null.asInstanceOf[Thread]
        }

      }
      // Park the current thread if needed
      // The current threads gets unparked if the predecessor changes or
      // the predecessor is marked as written
      if (waits) {
        LockSupport.park(predecessor.creatorThread)
      }
    }
    assert(predecessor == null || predecessor.isWritten)
  }

  protected[rescala] final def awaitUntilWritten() = {
    var wait = true
    while (wait) {
      lockObject.synchronized {
        wait = !isWritten
        lockedOnThread += Thread.currentThread()
      }
      LockSupport.park(creatorThread)
    }
  }

  protected[rescala] final def removeFrame() = {
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

  protected[rescala] final def moveAfter(newPredecessor: Frame[T]) = {
    assert(newPredecessor != null)
    removeFrame()
    insertAfter(newPredecessor)
  }

  protected[rescala] final def insertAfter(newPredecessor: Frame[T]) = {
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

case class WriteFrame[T](override val turn: Turn, override val at: PipelineBuffer) extends Frame[T](turn, at) {

}

case class DynamicReadFrame[T](override val turn: Turn, override val at: PipelineBuffer, val newDependent: Reactive) extends Frame[T](turn, at) {

}

case class DynamicDropFrame[T](override val turn: Turn, override val at: PipelineBuffer, val lostDependent: Reactive) extends Frame[T](turn, at) {

}

