package rescala.pipelining

import rescala.turns.Engine
import rescala.turns.Engines.EngineImpl
import rescala.graph.Reactive
import rescala.synchronization.TurnLock
import rescala.graph.SimpleBuffer
import rescala.graph.Buffer
import rescala.turns.Turn
import scala.collection.immutable.Queue
import java.util.concurrent.locks.LockSupport

/**
 * @author moritzlichter
 */

class PipelineEngine extends EngineImpl[PipeliningTurn]() {

  protected[pipelining] val stableTurn = makeTurn

  private type PTurn = PipeliningTurn

  import Pipeline._

  @volatile
  private var turnOrder = List[PTurn]()
  private object turnOrderLock

  private var completedNotRemovedTurns: Set[PTurn] = Set()
  private object completedNotRemovedTurnsLock

  protected[pipelining] def addTurn(turn: PTurn) = turnOrderLock.synchronized {
      val turnOrderWasEmpty = turnOrder.isEmpty
      turnOrder :+= turn
      if (turnOrderWasEmpty) {
        turn.needContinue()
      }
  }
  protected[pipelining] def getTurnOrder() = turnOrder

  protected[pipelining] def isActive(turn: PTurn) = turnOrder.contains(turn)

  protected[pipelining] def canTurnBeRemoved(turn: PTurn) = turnOrderLock.synchronized(turnOrder(0) == turn)

  protected def makeNewTurn = new PipeliningTurn(this)

  override final protected[pipelining] def makeTurn: PipeliningTurn = {
    val newTurn = makeNewTurn
    newTurn
  }

  /**
   * Creates a new frame for the given turn at the given reactive and
   * resolves conflicts which are introduced by creating the new frame
   */
  protected[pipelining] def createFrameBefore(turn: PTurn, at: Reactive) = {
    pipelineFor(at).createFrameBefore(turn)
  }

  protected[pipelining] def createFrameAfter(turn: PTurn, createFor: PTurn, at: Reactive): Boolean = {
    // TODO first check for conflicts
    // resolveConflicts(turn, at.getPipelineFrames().map { _.turn.asInstanceOf[PipeliningTurn]}.toSet)
    if (pipelineFor(at).hasFrame(createFor)) {
      // at has already a frame for createFor, dont create a new one
      // TODO assert that createFor is after turn in the pipeline
      assert(createFor >= turn)
      false
    } else {
      pipelineFor(at).insertWriteFrameFor(createFor)(turn)
      true
    }
  }

  /**
   * Implements a depth first search of the waiting graph to check
   * whether waits waits on on
   */
  protected[pipelining] def waitsOn(waits: PTurn, on: PTurn): Boolean = {
    // Dont need synchronization here, this makes every thing faster
    // new turns are appended to the turn order, but they are appended, before any waitsOn call involving it is creates
    // thus, if on is not contained in turnOrder, it has been removed because it was finished, thus was before waits
    // Not both indices can be null, at least one turn is active, otherwise no call was made
    val currentOrder = turnOrder
    assert(currentOrder.contains(waits))
    val waitsIndex = currentOrder.indexOf(waits)
    val onIndex = currentOrder.indexOf(on)
    if (onIndex == -1) {
      true
    } else if (waitsIndex == -1) {
      assert(false)
      false
    } else
      waitsIndex >= onIndex
  }

  protected[pipelining] def turnCompleted(completedTurn: PTurn): Unit = {
    import rescala.util.JavaFunctionsImplicits._

    turnOrderLock.synchronized {
      assert(turnOrder.contains(completedTurn))
      assert(turnOrder.head == completedTurn)
      turnOrder = turnOrder.tail
      if (!turnOrder.isEmpty) {
        turnOrder.head.needContinue()
      }
    }

  }

  override def bufferIncoming[A](default: A, commitStrategy: (A, A) => A, at: Reactive) = bufferBlocking[A](default, commitStrategy, at)
  override def bufferPulses[A](default: A, commitStrategy: (A, A) => A, at: Reactive) = bufferBlocking[A](default, commitStrategy, at)
  override def bufferOutgoing[A](default: A, commitStrategy: (A, A) => A, at: Reactive) = bufferNonblocking[A](default, commitStrategy, at)
  override def bufferLevel[A](default: A, commitStrategy: (A, A) => A, at: Reactive) = bufferNonblocking[A](default, commitStrategy, at)

  private def bufferBlocking[A](default: A, commitStrategy: (A, A) => A, at: Reactive): BlockingPipelineBuffer[A] = {
    assert(at != null)
    assert(pipelineFor(at) != null)
    pipelineFor(at).createBlockingBuffer(default, commitStrategy)
  }

  private def bufferNonblocking[A](default: A, commitStrategy: (A, A) => A, at: Reactive): NonblockingPipelineBuffer[A] = {
    assert(at != null)
    assert(pipelineFor(at) != null)
    pipelineFor(at).createNonblockingBuffer(default, commitStrategy)
  }

}