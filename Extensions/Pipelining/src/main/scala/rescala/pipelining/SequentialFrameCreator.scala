package rescala.pipelining

import rescala.graph.Reactive

private[pipelining] object SequentialFrameCreator {

  private object framingLock

}

private[pipelining] trait SequentialFrameCreator extends QueueBasedFrameCreator {

  self : PipeliningTurn =>

  override protected def createFrames(initialWrites: Traversable[Reactive[S]]): Unit = {
    SequentialFrameCreator.framingLock.synchronized{
      engine.addTurn(this)
      super.createFrames(initialWrites)
    }
  }

}
