package rescala.pipelining

import rescala.graph.Reactive

object SequentialFrameCreator {
  
  private object framingLock
  
}

trait SequentialFrameCreator extends QueueBasedFrameCreator {
  
  self : PipeliningTurn =>
    
  override protected[this] def createFrames(initialWrites : List[Reactive]) = {
    println(s"Sequentiel: new turn $this")
    SequentialFrameCreator.framingLock.synchronized{
      println(s"Sequentiel: begin framing for turn $this")
      engine.addTurn(this)
      val framedReactives = super.createFrames(initialWrites)
      println(s"Sequentail: framed turn $this at reactives $framedReactives")
      framedReactives 
    }
  }

}