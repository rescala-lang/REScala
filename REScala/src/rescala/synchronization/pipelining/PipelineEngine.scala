package rescala.synchronization.pipelining


/**
 * @author moritzlichter
 */
class PipelineEngine {
  
  type PTurn = PipeliningTurn
  type PTurns = Set[PipeliningTurn]
  
  var activeTurns : PTurns = Set()
  
  var waitingRelation: Map[PTurn, PTurns] = Map()
  
  def canWaitOn(turn: PTurn, other : PTurn) = other.synchronized {
   waitingRelation(other).contains(turn)
  }
  
  def waitOn(turn : PTurn, other : PTurn) : Unit = turn.synchronized {
    if (!waitingRelation.getOrElse(turn, Set()).contains(other)) {
      val currentWaits = waitingRelation.getOrElse(turn, Set())
      val newWaits = currentWaits ++ Set(other) ++ waitingRelation.getOrElse(other, Set()) 
      waitingRelation = waitingRelation + ((turn, newWaits))
    }
  }
  
}