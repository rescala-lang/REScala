package rescala.synchronization.pipelining

import rescala.graph.Reactive
import rescala.turns.Turn
import rescala.propagation.TurnImpl
import rescala.graph.Committable
import rescala.propagation.LevelQueue
import rescala.graph.ReactiveTurnData
import rescala.graph.ReactiveTurnData

class PipeliningTurn (engine : PipelineEngine) extends TurnImpl {
  
  // Need to encode the order of the turns
  
  class RemoveFrame(reactive : Reactive) extends Committable{
    
    override def commit(implicit turn: Turn): Unit = {
      reactive.tryRemoveFrame
    }
    override def release(implicit turn: Turn): Unit= {}
    
  }

  override def evaluate(head: Reactive) = {
    super.evaluate(head)
    head.markWritten
    schedule(new RemoveFrame(head))
  }
  
  private def createFrameForReactive(reactive : Reactive) : Unit =  {
    reactive.createFrame { d : ReactiveTurnData => {
      val otherTurn = d.turn.get.asInstanceOf[PipeliningTurn]
      if (engine.canWaitOn(this, otherTurn)) {
        engine.waitOn(this, otherTurn)
        true
      } else {
        false
      }
     }
    }
  }
  
  override def lockPhase(initialWrites: List[Reactive]): Unit = {
    val lq = new LevelQueue()
    initialWrites.foreach(lq.enqueue(-1))

    lq.evaluateQueue {createFrameForReactive(_)}
  }
  
}