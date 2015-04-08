package rescala.pipelining

import rescala.graph.Reactive
import rescala.turns.Turn
import rescala.propagation.TurnImpl
import rescala.graph.Committable
import rescala.propagation.LevelQueue
import rescala.graph.ReactiveFrame
import rescala.graph.ReactiveFrame
import rescala.Signal

class PipeliningTurn(override val engine: PipelineEngine, randomizeDeps : Boolean = false) extends TurnImpl {

  /**
   * Remember all reactives for which a frame was created during this turn
   */
  private var framedReactives : Set[Reactive] = Set()
  
  override def evaluate(head: Reactive) = {
    assert (head.hasFrame(this), "No frame was created in turn " + this  + " for " + head)
    while (! head.isPreviousFrameFinished) {
      // Prototype: Busy waiting 
    }
    println("Evaluate for turn " + this + " at "+ head)
   // println("Deps     " + head.outgoing.get )
   // println("val      " + {head.asInstanceOf[Signal[_]].get})
    head.fillFrame
    Thread.sleep(500)
   // println("New Deps " + head.outgoing.get)
   // println("New val  " + {head.asInstanceOf[Signal[_]].get})
    super.evaluate(head)
    println(s"   evaluated for $this to ${head.asInstanceOf[Signal[_]].get}at $head")
    // Mark the frame as written -> the turn will not touch this frame again
    head.markWritten
  }

  override def lockPhase(initialWrites: List[Reactive]): Unit = {
    val lq = new LevelQueue()
    initialWrites.foreach(lq.enqueue(-1))

    // Create frames for all reachable reactives
    lq.evaluateQueue { reactive =>
      println ("Lock phase for turn " + this + " at  "+ reactive + " in thread" + Thread.currentThread().getId)
      Thread.sleep(500)
      engine.createFrame(this, reactive)
      assert(reactive.hasFrame(this))
      framedReactives += reactive
      reactive.outgoing.get.foreach { lq.enqueue(-1) }
    }
  }
  
  override def releasePhase(): Unit = {
    // Mark all frames for removal
    framedReactives.foreach { _.tryRemoveFrame }
  }

}