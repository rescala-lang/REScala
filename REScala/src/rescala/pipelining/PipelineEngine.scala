package rescala.pipelining

import rescala.turns.Engine
import rescala.turns.Engines.EngineImpl
import rescala.graph.Reactive


/**
 * @author moritzlichter
 */
class PipelineEngine extends EngineImpl[PipeliningTurn](){
  
  private type PTurn = PipeliningTurn
  
  /**
   * A Map which stores for a mapping (t1, t2) -> rs, that
   * turn t1 is before turn t2 at the reactives rs
   */
  // TODO need to cleanup the map if turns are done
  private var ordering: Map[(PTurn, PTurn), Set[Reactive]] = Map()
  /**
   * A map which tracks which turn waits for which other. If t1 -> ts,
   * then for all t in ts an entry (t1, t) in ordering exists, which
   * does not map to an empty set (or is not defined)
   */
  private var waitingEdges : Map[PTurn, Set[PTurn]] = Map()
  
  protected [pipelining] def getOrdering = ordering
  protected [pipelining] def getWaitingEdges = waitingEdges
  
  // For debugging
  override protected[pipelining] def makeTurn : PipeliningTurn = new PipeliningTurn(this)
  
  /**
   * Creates a new frame for the given turn at the given reactive and
   * resolves conflicts which are introduced by creating the new frame
   */
  protected[pipelining] def createFrame(turn : PTurn, at : Reactive) = {
    println("Create frame for " + turn + " at " + at)
    at.createFrame { frame =>
      val before = frame.turn.asInstanceOf[PipeliningTurn]
      // Only one node at a time can modify ordering
      // TODO: can allow to create frames in parallel if possible?
      ordering.synchronized {
        // First resolve conflicts which would create a cycle
        resolveConflicts(before, turn)
        // Then remember the new turn
        rememberOrder(before, turn, at)
      }
      true
    } (turn)
  }
 
  private def rememberOrder(before: PTurn, after : PTurn, at : Reactive) = {
    if (before == after)
      throw new AssertionError
    val forcingReactives = ordering.getOrElse((before, after), Set()) + at
    ordering = ordering + ((before, after) -> forcingReactives)
    println ("Remember (" + before + ", " + after + ") at " + at)
    val waiting = waitingEdges.getOrElse(after, Set()) + before
    waitingEdges = waitingEdges + (after -> waiting)
    println("Waits " + after + " on " + waiting)
  }
  
  private def waitsOn(waits : PTurn, on : PTurn) : Boolean = {
    if (waits == on)
      true
    else {
      waitingEdges.getOrElse(waits, Set()).exists { waitsOn(_, on)}
    }
  }
  
  private def getConflicts(before : PTurn, after : PTurn) : List[(PTurn, Set[Reactive])] = {
    println("Before " + before + "  after " + after)
    println(waitingEdges)
    val directWaits = waitingEdges.keySet.filter { waitingEdges(_).contains(after) };//.getOrElse(after, Set())
    println ("Direct waits: " + directWaits)
    val directWaitsInCircle = directWaits.filter { waitsOn(before, _) }.toList
    println ("Cycle       : " + directWaitsInCircle)
    val conflictedReactives = directWaitsInCircle.map {before =>  ordering.getOrElse((after, before), Set())}
    println ("Reactives   : " + conflictedReactives)
    directWaitsInCircle.zip(conflictedReactives)
  }
   
  private def resolveConflicts(before : PTurn, after : PTurn) = {
    def resolveConflict(before : PTurn, after : PTurn, at : Reactive) = {
      at.moveFrameBack { frame =>
        val before2 = frame.turn.asInstanceOf[PTurn] 
        ordering = ordering + ((after, before2) -> (ordering((after, before2)) - at))
        ordering = ordering + ((before2, after) -> (ordering.getOrElse((before2, after), Set()) + at))
        waitingEdges = waitingEdges + (after -> (waitingEdges.getOrElse(after, Set()) + before2))
        if (after == before2) {
          println("NOOOOOOOOOOOOOOO")
        }
        if (ordering((after, before2)).isEmpty) {
          ordering = ordering - ((after, before2))
          val waitings = waitingEdges.getOrElse(before2, Set()) - after
          if (waitings.isEmpty)
            waitingEdges = waitingEdges - before2
          else
            waitingEdges = waitingEdges + (before2 -> waitings)
        }  
        before2 == before }(after)
    }
    
    val conflicts = getConflicts(before, after)
    
    conflicts.foreach( conflict => conflict match{
      case (turn, reactives) =>
        reactives.foreach{ reactive =>
          println("Resolve turn " + after + " move after " + turn + " at " + reactive)
          resolveConflict(turn, after, reactive)
        }
        if (ordering.contains((after, turn))) {
          throw new AssertionError("Created a cycle")
        }    
    })
    println(waitingEdges)
    
 
  }
  
  
}