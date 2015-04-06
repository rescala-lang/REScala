package rescala.graph

import rescala.turns.Turn
import rescala.synchronization.TurnLock
import rescala.turns.Engine
import rescala.turns.Ticket
import rescala.graph.Pulse.{Diff, NoChange}
import scala.collection.immutable.Queue

/** A Reactive is something that can be reevaluated */
trait Reactive {
  protected[this] type D <: ReactiveTurnData;
  final override val hashCode: Int = Globals.nextID().hashCode()

  protected[rescala] def engine: Engine[Turn]
  
   protected[rescala] def lock: TurnLock 
  
  protected def initialStableFrame : D;
  protected def newFrameFrom(turn: Turn, other : D) : D;
  private [rescala] var stableFrame : D = initialStableFrame
  private[rescala] var pipelineFrames : Queue[D] = Queue()

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
  /*
  protected [rescala] def moveFrame(currentTurn : Turn, before : Turn) : Unit = {
    val currentFrame = frame()(currentTurn)
    @tailrec def moveFrameImpl(queue: Queue[D]) : Queue[D] = queue match{
      case head :+ rest =>
        if (head == before)
          currentFrame :+ (head :+ moveFrameImpl(rest))
        else if (head == currentFrame)
          moveFrameImpl(rest)
        else
          head :+ moveFrameImpl(rest)
      case Queue() => Queue()
    }
  }*/
  
  protected def frame[T]( f : D => T = {x:D => x})( implicit turn : Turn) : T = {
    pipelineFrames.find { x => x.turn.get eq turn} match {
      case Some(d) => f(d)
      case None => f(stableFrame) //throw new AssertionError(s"No Data for turn $turn at node $this")
    }
  }
  
  private def createFrame(turn : Turn) : Unit = {
    val latestFrame = if (pipelineFrames.isEmpty)
      stableFrame
    else
      pipelineFrames.last
    val newFrame = newFrameFrom(turn, latestFrame)
    pipelineFrames = pipelineFrames :+ newFrame
  }
  
  // If want to omit the buffers in the turn data (because the previous data is contained
  // in the frame before), I can only remove the head turn in the queue and need to remove
  // other turns only if they get head in the queue
  // Then I need to store in the Turn whether it is completed
  
  private def finishTurn( turn : Turn) : Unit = {
    def removeTurn (queue : Queue[D]) : Queue[D] = {
      if (queue.isEmpty) {
        queue
      } else {
        val head = queue.head
        if (head.turn.get eq turn)
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


/** A node that has nodes that depend on it */
trait Pulsing[+P] extends Reactive {
  protected [this] override type D <:PulsingTurnData[P]
  final def pulse(implicit turn: Turn): Pulse[P] = frame(_.pulses).get
  protected[this] def pulses(implicit turn:Turn): Buffer[Pulse[P]] = frame(_.pulses)
}


/** a node that has a current state */
trait Stateful[+A] extends Pulsing[A] {
  protected [this] override type D <:StatefulTurnData[A]

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