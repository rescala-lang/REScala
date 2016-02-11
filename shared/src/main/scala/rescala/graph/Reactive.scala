package rescala.graph

import rescala.turns.Turn
import rescala.synchronization.TurnLock
import rescala.turns.Engine
import rescala.turns.Ticket
import rescala.graph.Pulse.{Diff, NoChange}
import scala.collection.immutable.Queue
import rescala.pipelining.Pipeline
import rescala.pipelining.PipelineEngine
/** A Reactive is something that can be reevaluated */
/*
trait PipeReactive  {
  
  final override val hashCode: Int = Globals.nextID().hashCode()
  
  protected[rescala] def engine: Engine[Turn]
  protected[rescala] def lock: TurnLock
  private[rescala] final val pipeline = if (engine.isInstanceOf[PipelineEngine]) new Pipeline(this) else null
  
  private[rescala] final val level : Buffer[Int] = engine.bufferLevel(0, math.max, this)
  private[rescala] final val outgoing : Buffer[Set[Reactive]] = engine.bufferOutgoing(Set(), Buffer.commitAsIs, this)
  private[rescala] val incoming : Buffer[Set[Reactive]] = engine.bufferIncoming(Set(), Buffer.commitAsIs, this)
  
  
  

  /** called when it is this events turn to be evaluated
    * (head of the evaluation queue) */
  protected[rescala] def reevaluate()(implicit turn: Turn): ReevaluationResult

  /** for debugging */
  private val name = Globals.declarationLocationName()
  override def toString = s"$name[id=$hashCode]"
}

*/