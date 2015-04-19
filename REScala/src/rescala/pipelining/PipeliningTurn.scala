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
  
  override def evaluate(head: Reactive) = {
    assert (head.hasFrame(this), "No frame was created in turn " + this  + " for " + head)
    while (! head.isPreviousFrameFinished) {
      // Prototype: Busy waiting 
    }
    //println("Evaluate for turn " + this + " at "+ head)
   // println("Deps     " + head.outgoing.get )
   // println("val      " + {head.asInstanceOf[Signal[_]].get})
    // Hack dont override changes to sources => need to do that better
    if (head.incoming.nonEmpty)
      head.fillFrame
      
   // println("New Deps " + head.outgoing.get)
   // println("New val  " + {head.asInstanceOf[Signal[_]].get})
    super.evaluate(head)
    //val sig = head.asInstanceOf[Signal[_]]
    //println(s"   evaluated for $this to ${sig.get}at $head")
    //println(s"   with frame for $this ${sig.getPipelineFrames().map { p => s"[${p.turn} = ${p.pulses.get}]" }.mkString(",")}")
    // Mark the frame as written -> the turn will not touch this frame again
    head.markWritten
  }

  override def lockPhase(initialWrites: List[Reactive]): Unit = {
    val lq = new LevelQueue()
    initialWrites.foreach(lq.enqueue(-1))

    // Create frames for all reachable reactives
    lq.evaluateQueue { reactive =>
      //println ("Lock phase for turn " + this + " at  "+ reactive + " in thread" + Thread.currentThread().getId)
      engine.createFrame(this, reactive)
      assert(reactive.hasFrame(this))
      framedReactives += reactive
      reactive.outgoing.get.foreach { lq.enqueue(-1) }
    }
  }
  
  override def releasePhase(): Unit = {
    engine.turnCompleted(this)
  }

  override def toString =  {
    s"PipeliningTurn(${super.toString})"
  }
  
}