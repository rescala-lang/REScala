package rescala.propagation

import rescala.graph.Reactive
import rescala.graph.ReevaluationResult
import rescala.graph.ReevaluationResult.{Static, Dynamic}

trait PropagateNoChanges {
  
  self : TurnImpl => 
   
   protected def evaluateNoChange(head : Reactive) : QueueAction

   override protected def calculateQueueAction(head : Reactive, result : ReevaluationResult) = {
     result match {
      case Static(hasChanged) =>
        (hasChanged, -1, EnqueueDependencies)
      case Dynamic(hasChanged, diff) =>
        diff.removed foreach unregister(head)
        diff.added foreach register(head)
        val newLevel = maximumLevel(diff.novel) + 1
        val action = if (head.level.get < newLevel) RequeueReactive else EnqueueDependencies
        (hasChanged, newLevel,  action)
    }
   }
   
   def propagationPhase(): Unit = levelQueue.evaluateQueue(evaluate, evaluateNoChange)


}