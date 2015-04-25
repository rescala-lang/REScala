package rescala.pipelining

import rescala.graph.Reactive
import rescala.turns.Turn
import rescala.propagation.TurnImpl
import rescala.graph.Committable
import rescala.propagation.LevelQueue
import rescala.graph.ReactiveFrame
import rescala.graph.ReactiveFrame
import rescala.Signal
import rescala.graph.Pulsing

class PipeliningTurn(override val engine: PipelineEngine, randomizeDeps : Boolean = false) extends TurnImpl {

  /**
   * Remember all reactives for which a frame was created during this turn
   */
  protected [pipelining] var framedReactives : Set[Reactive] = Set()
  
  override def waitsOnFrame(other : Turn) = other == this || engine.waitsOn(this, other.asInstanceOf[PipeliningTurn])
  
  override def waitsOnLock[T](op : => T) : T = engine.graphLocked(op)
  
  
  override def evaluate(head: Reactive) = {
    assert (head.hasFrame(this), "No frame was created in turn " + this  + " for " + head)
    
    head.waitUntilCanWrite
    head.markTouched
    
    // Hack dont override changes to sources => need to do that better
    if (head.incoming.nonEmpty)
      head.fillFrame
      
    super.evaluate(head)
    
    head.markWritten
  }

  
  // lock phases cannot run in parrallel currently,......
  override def lockPhase(initialWrites: List[Reactive]): Unit = {engine.synchronized{
    def createFrame(reactive : Reactive) : Unit =  {
      engine.createFrame(this, reactive)
      assert(reactive.hasFrame(this)) 
      framedReactives += reactive
    }
    
    val lq = new LevelQueue()
    initialWrites.foreach {createFrame(_) }
    initialWrites.foreach(lq.enqueue(-1))

    // Create frames for all reachable reactives
    lq.evaluateQueue { reactive => 
      val outgoings = reactive.outgoing.get
      outgoings.foreach(createFrame(_))
      outgoings.foreach { lq.enqueue(-1) } 
    }
  }}
  
  override def releasePhase(): Unit = {
    framedReactives.foreach(_.markWritten)
    engine.turnCompleted(this)
  }

  override def toString =  {
    s"PipeliningTurn(${super.toString})"
  }
  
}