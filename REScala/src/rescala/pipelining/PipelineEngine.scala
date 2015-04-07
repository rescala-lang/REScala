package rescala.pipelining

import rescala.turns.Engine
import rescala.turns.Engines.EngineImpl
import rescala.graph.Reactive


/**
 * @author moritzlichter
 */
class PipelineEngine extends EngineImpl[PipeliningTurn](){
  
  type PTurn = PipeliningTurn
  
  /**
   * A Map which stores for a mapping (t1, t2) -> rs, that
   * turn t1 is before turn t2 at the reactives rs
   */
  // TODO need to cleanup the map if turns are done
  private var ordering: Map[(PTurn, PTurn), Set[Reactive]] = Map()
  
  override protected[pipelining] def makeTurn : PipeliningTurn = new PipeliningTurn(this)
  
  /**
   * Creates a new frame for the given turn at the given reactive and
   * resolves conflicts which are introduced by creating the new frame
   */
  protected[pipelining] def createFrame(turn : PTurn, at : Reactive) = {
    at.createFrame { frame => {
      val before = frame.turn.asInstanceOf[PipeliningTurn]
      // Only one node at a time can modify ordering
      // TODO: can allow to create frames in parallel if possible?
      ordering.synchronized {
        rememberOrder(before, turn, at)
        resolveConflicts(before, turn)
      }
      true
    } } (turn)
  }
 
  private def rememberOrder(before: PTurn, after : PTurn, at : Reactive) = {
    val forcingReactives = ordering.getOrElse((before, after), Set()) + at
    ordering = ordering + ((before, after) -> forcingReactives)
  }
  
  private def getConflicts(before : PTurn, after : PTurn) : Set[Reactive] = {
    ordering.getOrElse((after, before), Set())
  }
  
  private def resolveConflicts(before : PTurn, after : PTurn) = {
    def resolveConflict(before : PTurn, after : PTurn, at : Reactive) = {
      at.moveFrameBack { frame => frame.turn == before }(after)
    }
    
    val conflictingReactives = getConflicts(before, after)
    conflictingReactives.foreach { resolveConflict(before, after, _)}
    ordering = ordering - ((after, before))
  }
  
  
}