package rescala.pipelining

import rescala.graph.Reactive
import rescala.pipelining.util.TransferableLock

private[pipelining] object ParallelFrameCreator {

  // Todo each engine should have its own copied of this values
  
  private object turnOrderLock
  private var turnOrder = List[PipeliningTurn]()

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

private[pipelining] trait ParallelFrameCreator extends QueueBasedFrameCreator {

  self: PipeliningTurn =>

  override protected[this] def createFrame(pipeline: Pipeline) : Unit =  pipeline.createFrameBefore(this)
    
  override protected[this] def createFrames(initialWrites: List[Reactive[PipelineStruct.type]]) = {
    ParallelFrameCreator.addTurn(this)
    super.createFrames(initialWrites)
    ParallelFrameCreator.removeTurn(this)

  }

}