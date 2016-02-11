package rescala.propagation

import rescala.graph.Reactive
import rescala.graph.ReevaluationResult
import rescala.graph.ReevaluationResult.{Static, Dynamic}

trait PropagateChangesOnly {
  
  self : TurnImpl => 

   override protected def calculateQueueAction(head : Reactive, result : ReevaluationResult) : (Boolean, Int, QueueAction) = 
     result match {
      case Static(true) =>
        (true, -1, EnqueueDependencies)
      case Static(false) =>
        (false, -1, DoNothing)
      case Dynamic(hasChanged, diff) =>
        diff.removed foreach unregister(head)
        diff.added foreach register(head)
        val newLevel = maximumLevel(diff.novel) + 1
        if (head.level.get < newLevel) 
          (hasChanged, newLevel, RequeueReactive)
        else if (hasChanged)
          (true, newLevel, EnqueueDependencies)
        else (false, newLevel, DoNothing)
    }
   
   def propagationPhase(): Unit = levelQueue.evaluateQueue(evaluate)


}