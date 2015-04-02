package rescala.graph

import rescala.graph.Pulse.{Diff, NoChange}
import rescala.synchronization.{TurnLock, Key}
import rescala.turns.{Engine, Ticket, Turn}
import scala.collection.immutable.Queue
import scala.annotation.tailrec

/** A Reactive is something that can be reevaluated */
trait Reactive[D <: ReactiveTurnData[D]] {
  final override val hashCode: Int = Globals.nextID().hashCode()

  protected[rescala] def engine: Engine[Turn]
  
  protected[rescala] def lock: TurnLock // Needed?
  
  private [rescala] var stableFrame : D
  private[rescala] var pipelineFrames : Queue[D]

  final private[rescala] def level (implicit turn: Turn) = frame(_.level)
  final private[rescala] def outgoing (implicit turn: Turn) = frame(_.outgoing)
  protected[rescala] def incoming(implicit turn: Turn) = frame(_.incoming)
  
  // Does pipelining in this way remove STM support? If yes, is that a problem if 
  // we know now, that it is not that useful?
  
  // TODO add synchronization for thread safe access to queue
  
  // Locking phase as usual, but reactives can be locked if current frame is written
  //   In the locking phase, after all locks could be gained, insert new frames for each
  //   involved reactive => other turns cannot lock them until the frames were set to be written
  //   
  
  protected def frame[T]( f : D => T )( implicit turn : Turn) : T = {
    pipelineFrames.find { x => x.turn eq turn} match {
      case Some(d) => f(d)
      case None => throw new AssertionError(s"No Data for turn $turn at node $this")
    }
  }
  
  // todo TODO implement this in the lock, like now it is wrong
  // Maybe need to store the key in the TurnData
  
  private def tryLock(key : Key, turn : Turn) : Boolean = {
    if (lock.tryLock(key) == key)
      true
    else if (pipelineFrames.isEmpty) 
      // This should not be able to ocur because there is a turn so at least one frame
      throw new AssertionError("Cannot be locked without any frame")
    else if (pipelineFrames.last.isWritten())
      // Can be acquired because the last turn already wrote
      true
    else
      false
  }
  
  private def createFrame(turn : Turn) : Unit = {
    val latestFrame = if (pipelineFrames.isEmpty)
      stableFrame
    else
      pipelineFrames.last
    val newFrame = latestFrame.copy
    pipelineFrames = pipelineFrames :+ newFrame
  }
  
  // If want to omit the buffers in the turn data (because the previous data is contained
  // in the frame before), I can only remove the head turn in the queue and need to remove
  // other turns only if they get head in the queue
  // Then I need to store in the Turn whether it is completed
  
  private def finishTurn( turn : Turn) : Unit = {
    @tailrec def removeTurn (queue : Queue[D]) : Queue[D] = {
      if (queue.isEmpty) {
        queue
      } else {
        val head = queue.head
        if (head.turn eq turn)
          queue
        else if (!head.isWritten()) 
          throw new AssertionError("A turn could not be added if any preceeding has not written")
        else
          head +: removeTurn(queue)
      }
    }
    pipelineFrames = removeTurn(pipelineFrames)
  }
  

  /** called when it is this events turn to be evaluated
    * (head of the evaluation queue) */
  protected[rescala] def reevaluate()(implicit turn: Turn): ReevaluationResult

  /** for debugging */
  private val name = Globals.declarationLocationName()
  override def toString = name
}


/** helper class to initialise engine and select lock */
abstract class Enlock(final override protected[rescala] val engine: Engine[Turn],
                      knownDependencies: Set[Reactive[_]] = Set.empty) extends Reactive {
  final override protected[rescala] val lock: TurnLock =
    if (knownDependencies.size == 1) knownDependencies.head.lock
    else new TurnLock(this)

  def staticIncoming: Set[Reactive[_]] = knownDependencies
}


/** A node that has nodes that depend on it */
trait Pulsing[+P, D<: PulsingTurnData[P,D]] extends Reactive[D] {
 
  final def pulse(implicit turn: Turn): Pulse[P] = frame(_.pulses).get
}


/** a node that has a current state */
trait Stateful[+A] extends Pulsing[A] {
  pulses.initStrategy(Buffer.keepPulse)

  // only used inside macro and will be replaced there
  final def apply(): A = throw new IllegalAccessException(s"$this.apply called outside of macro")

  final def apply[T](turn: Turn): A = {
    turn.accessDynamic(this)
    Globals.useDependency(this)
    get(turn)
  }

  final def now(implicit maybe: Ticket): A = maybe { get(_) }

  final def get(implicit turn: Turn): A = pulse match {
    case NoChange(Some(value)) => value
    case Diff(value, oldOption) => value
    case NoChange(None) => throw new IllegalStateException("stateful reactive has never pulsed")
  }
}

