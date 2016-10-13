package rescala.pipelining

import rescala.graph.Reactive

private[pipelining] object SequentialFrameCreator {
  
  private object framingLock
  
}

private[pipelining] trait SequentialFrameCreator extends QueueBasedFrameCreator {
  
  self : PipeliningTurn =>
    
  override protected[this] def createFrames(initialWrites : List[Reactive[PipelineStruct.type]]) = {
    SequentialFrameCreator.framingLock.synchronized{
      engine.addTurn(this)
      super.createFrames(initialWrites)
    }
  }

}