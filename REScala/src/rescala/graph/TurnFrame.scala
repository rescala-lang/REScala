package rescala.graph

import rescala.synchronization.{ TurnLock }
import rescala.turns.Turn
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.locks.Condition
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.locks.LockSupport
import java.util.concurrent.atomic.AtomicReference
import rescala.util.JavaFunctionsImplicits._

class TurnFrame(_turn: Turn) {

  private val creatorThread = Thread.currentThread()
  private val lockObject = new Object
  private var lockedOnThread: Set[Thread] = Set()

  private var predecessor: TurnFrame = null.asInstanceOf[TurnFrame]
  private var successor: TurnFrame = null.asInstanceOf[TurnFrame]
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

  protected[rescala] final def awaitPredecessor() = {
    var waits = predecessor != null
    while (waits) {
      predecessor.lockObject.synchronized {
        waits = predecessor != null && !predecessor.isWritten()
        if (waits) {
          predecessor.lockedOnThread += Thread.currentThread()
        }
      }
      if (waits) {
        LockSupport.park(predecessor.creatorThread)
      }
    }
  }

  protected[rescala] final def removeFrame() = {
    if (successor != null) {
      successor.predecessor = this.predecessor
    }
    if (predecessor != null) {
      predecessor.successor = this.successor
    }
    val oldSuccessor = this.successor
    this.successor = null
    this.predecessor = null

    if (oldSuccessor != null) {
      oldSuccessor.retryBlockedThreads()
    }
  }

  protected[rescala] final def moveAfter(newPredecessor: TurnFrame) = {
    assert(newPredecessor != null)
    removeFrame()
    insertAfter(newPredecessor)
  }

  protected[rescala] final def insertAfter(newPredecessor: TurnFrame) = {
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

class ReactiveFrame(
  _turn: Turn,
  r: Reactive,
  _level: Buffer[Int],
  _outgoing: Buffer[Set[Reactive]],
  _incoming: Set[Reactive])
  extends TurnFrame(_turn) {

  def this(_turn: Turn, r: Reactive, _incoming: Set[Reactive]) {
    this(_turn, r, r.engine.buffer(0, math.max, r.lock), r.engine.buffer(Set(), Buffer.commitAsIs, r.lock), _incoming)
  }

  private[rescala] val level: Buffer[Int] = _level

  private[rescala] val outgoing: Buffer[Set[Reactive]] = _outgoing

  protected[rescala] var incoming: Set[Reactive] = _incoming

}

class PulsingFrame[P](
  _turn: Turn,
  p: Pulsing[P],
  _level: Buffer[Int],
  _outgoing: Buffer[Set[Reactive]],
  _incoming: Set[Reactive],
  _pulses: Buffer[Pulse[P]])
  extends ReactiveFrame(_turn, p, _level, _outgoing, _incoming) {

  def this(_turn: Turn, p: Pulsing[P], _incoming: Set[Reactive]) {
    this(_turn, p, p.engine.buffer(0, math.max, p.lock), p.engine.buffer(Set(), Buffer.commitAsIs, p.lock), _incoming, p.engine.buffer(Pulse.none, Buffer.transactionLocal, p.lock))
  }

  protected[rescala] val pulses: Buffer[Pulse[P]] = _pulses
}

class StatefulFrame[A](
  _turn: Turn,
  p: Stateful[A],
  _level: Buffer[Int],
  _outgoing: Buffer[Set[Reactive]],
  _incoming: Set[Reactive],
  _pulses: Buffer[Pulse[A]])
  extends PulsingFrame[A](_turn, p, _level, _outgoing, _incoming, _pulses) {

  def this(_turn: Turn, p: Stateful[A], _incoming: Set[Reactive]) {
    this(_turn, p, p.engine.buffer(0, math.max, p.lock), p.engine.buffer(Set(), Buffer.commitAsIs, p.lock), _incoming, p.engine.buffer(Pulse.none, Buffer.transactionLocal, p.lock))
  }

  pulses.initStrategy(Buffer.keepPulse)

  override def toString = super.toString() + s"[turn=$turn, written=$isWritten, pulses=${pulses.get(null)}]"
}