package rescala.graph

import rescala.synchronization.{ TurnLock }
import rescala.turns.Turn
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.locks.Condition
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.locks.LockSupport
import java.util.concurrent.atomic.AtomicReference
import rescala.util.JavaFunctionsImplicits._

class TurnFrame[T <: TurnFrame[T]](_turn: Turn) {
  self: T =>

  private val creatorThread = Thread.currentThread()
  private val lockObject = new Object
  private var lockedOnThread: Set[Thread] = Set()

  private var predecessor: T = null.asInstanceOf[T]
  private var successor: T = null.asInstanceOf[T]
  private val written = new AtomicBoolean(false);
  private val toRemove = new AtomicBoolean(false);
  private var currentTurn = _turn

  protected[rescala] final def turn: Turn = currentTurn;

  protected[rescala] final def isWritten(): Boolean = written.get
  protected[rescala] final def removeTurn() = currentTurn = null
  protected[rescala] final def markWritten() = {
    lockObject.synchronized {
      written.set(true)
      retryBlockedThreads()
    }
  }

  private def retryBlockedThreads() = lockObject.synchronized {
    val blockedThreads = lockedOnThread
    lockedOnThread = Set()
    blockedThreads.foreach { LockSupport.unpark(_) }
  }

  protected[rescala] final def next() = successor
  protected[rescala] final def previous() = predecessor
  
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
            waits = !predecessor.isWritten()
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
        LockSupport.park(creatorThread)
      }
    }
  }

  protected[rescala] final def replaceWith(newFrame: T) = {
    newFrame.successor = this.successor
    newFrame.predecessor = this.predecessor
    if (this.predecessor != null) {
      this.predecessor.successor = newFrame
    }
    if (this.successor != null) {
      this.successor.predecessor = newFrame
    }
    this.successor = null.asInstanceOf[T]
    this.predecessor = null.asInstanceOf[T]

    newFrame.lockObject.synchronized {
      lockObject.synchronized {
        newFrame.lockedOnThread = lockedOnThread
      }
    }

    if (newFrame.successor != null) {
      newFrame.successor.retryBlockedThreads()
    }
    newFrame.retryBlockedThreads()
  }

  protected[rescala] final def removeFrame() = {
    if (successor != null) {
      successor.predecessor = this.predecessor
    }
    if (predecessor != null) {
      predecessor.successor = this.successor
    }
    val oldSuccessor = this.successor
    this.successor = null.asInstanceOf[T]
    this.predecessor = null.asInstanceOf[T]

    if (oldSuccessor != null) {
      oldSuccessor.retryBlockedThreads()
    }
  }

  protected[rescala] final def moveAfter(newPredecessor: T) = {
    assert(newPredecessor != null)
    removeFrame()
    insertAfter(newPredecessor)
  }

  protected[rescala] final def insertAfter(newPredecessor: T) = {
    assert(successor == null)

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
