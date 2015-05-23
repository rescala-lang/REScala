package rescala.pipelining

import rescala.turns.Engine
import rescala.turns.Engines.EngineImpl
import rescala.graph.Reactive
import rescala.synchronization.TurnLock
import rescala.graph.SimpleBuffer
import rescala.graph.Buffer
import rescala.turns.Turn
import scala.collection.immutable.Queue

/**
 * @author moritzlichter
 */


class PipelineEngine extends EngineImpl[PipeliningTurn]() {

  private type PTurn = PipeliningTurn
  
  import PipelineBuffer._

  private var turnOrder = List[PTurn]()
  private object turnOrderLock

  private var completedNotRemovedTurns: Set[PTurn] = Set()
  private object completedNotRemovedTurnsLock


  protected[pipelining] def addTurn(turn: PTurn) = turnOrderLock.synchronized(turnOrder :+= turn)
  protected[pipelining] def getTurnOrder() = turnOrder

  protected def makeNewTurn = new PipeliningTurn(this)

  override final protected[pipelining] def makeTurn: PipeliningTurn = {
    val newTurn = makeNewTurn
    newTurn
  }
  


  /**
   * Creates a new frame for the given turn at the given reactive and
   * resolves conflicts which are introduced by creating the new frame
   */
  protected[pipelining] def createFrame(turn: PTurn, at: Reactive) = {
    pipelineFor(at).createFrame(turn)
  }

  /**
   * Creates a new frame for the given turn at the given reactive and
   * resolves conflicts which are introduced by creating the new frame
   */
  protected[pipelining] def createFrameBefore(turn: PTurn, at: Reactive) = {
    pipelineFor(at).createFrameBefore(otherTurn => otherTurn > turn )(turn)
  }

  /**
   * Creates a new frame for the given turn at the given reactive and
   * resolves conflicts which are introduced by creating the new frame
   */
  protected[pipelining] def createDynamicReadFrameFrame(turn: PTurn, from: Reactive, at: Reactive) = {
    val frame = pipelineFor(at).createDynamicReadFrame(from)(turn)
    frame
  }

  protected[pipelining] def createFrameAfter(turn: PTurn, createFor: PTurn, at: Reactive): Boolean = {
    // TODO first check for conflicts
    // resolveConflicts(turn, at.getPipelineFrames().map { _.turn.asInstanceOf[PipeliningTurn]}.toSet)
    if (pipelineFor(at).hasFrame(createFor))
      // at has already a frame for createFor, dont create a new one
      // TODO assert that createFor is after turn in the pipeline
      false
    else {
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
    }
    else if (waitsIndex == -1) {
      assert(false)
      false
    }
    else
      waitsIndex >= onIndex
  }

  private def removeTurn(implicit turn : PTurn) : Unit = {
    turn.framedReactives.get.foreach {pipelineFor(_).removeFrames}
    assert(turn.framedReactives.get.forall { !pipelineFor(_).hasFrame(turn) })
  }
  
  protected[pipelining] def turnCompleted(completedTurn: PTurn): Unit = {
    import rescala.util.JavaFunctionsImplicits._

      turnOrderLock.synchronized {
        assert(turnOrder.contains(completedTurn))
        if (turnOrder.head == completedTurn) {
          removeTurn(completedTurn)
          turnOrder = turnOrder.tail
          while(turnOrder.nonEmpty && completedNotRemovedTurns.contains(turnOrder.head)) {
            removeTurn(turnOrder.head)
            turnOrder = turnOrder.tail
          }
        } else {
          completedNotRemovedTurns += completedTurn
        }
      }
    
  }
  
  override def buffer[A](default: A, commitStrategy: (A, A) => A, at : Reactive, takePrevious : Boolean): Buffer[A] = {
    assert(at != null)
    assert(at.pipeline != null)
    at.pipeline.createBuffer(default, commitStrategy, takePrevious)
  }

}