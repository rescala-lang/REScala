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

  protected def addTurn(turn: PipeliningTurn): List[PipeliningTurn] = turnOrderLock.synchronized {
    println(s"Parallel: add turn $turn")
    turn.engine.addTurn(turn)
    val turnsBefore = turnOrder
    if (turnsBefore.isEmpty)
      completeLock.reserveLockFor(turn.thread)
    turnOrder :+= turn
    turnsBefore
  }

  protected def removeTurn(turn: PipeliningTurn): Unit = {

    completeLock.lock()

    turnOrderLock.synchronized {
      println(s"Parallel: remove turn $turn")
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
    val turnsThatNeedToCompleteFramingBefore = ParallelFrameCreator.addTurn(this).toSet

    var framedReactives = Set[Reactive]()

    evaluateQueue(initialWrites) { reactive =>
      engine.createFrameBefore(this, ParallelFrameCreator.turnOrder.toSet -- turnsThatNeedToCompleteFramingBefore, reactive)
      framedReactives += reactive
    }

    ParallelFrameCreator.removeTurn(this)

    framedReactives

  }

}