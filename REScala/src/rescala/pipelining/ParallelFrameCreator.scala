package rescala.pipelining

import rescala.graph.Reactive
import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.atomic.AtomicReference
import rescala.util.TransferableLock
import rescala.util.TransferableLock
import java.awt.datatransfer.Transferable
import rescala.util.TransferableLock

object ParallelFrameCreator {

  private object turnOrderLock
  protected var turnOrder = List[PipeliningTurn]()

  private val completeLock = new TransferableLock

  protected def addTurn(turn: PipeliningTurn) = turnOrderLock.synchronized {
    turn.engine.addTurn(turn)
    if (turnOrder.isEmpty)
      completeLock.reserveLockFor(turn.thread)
    turnOrder :+= turn
  }

  protected def removeTurn(turn: PipeliningTurn): Unit = {

    completeLock.lock()

    turnOrderLock.synchronized {
      assert(turnOrder.head == turn)
      turnOrder = turnOrder.tail
      if (turnOrder.nonEmpty)
        completeLock.reserveLockFor(turnOrder.head.thread)
      else
        completeLock.unlock()
    }
  }
}

trait ParallelFrameCreator extends QueueBasedFrameCreator {

  self: PipeliningTurn =>

  override protected[this] def createFrames(initialWrites: List[Reactive]) = {
    ParallelFrameCreator.addTurn(this)

    val writeLock = framedReactivesLock.writeLock()
    writeLock.lock()
    evaluateQueue(initialWrites) { reactive =>
      markReactiveFramed(reactive, reactive => engine.createFrameBefore(this, reactive))
    }
    writeLock.unlock()

    ParallelFrameCreator.removeTurn(this)

  }

}