package rescala.pipelining.propagation

import rescala.graph.{Reactive, ReevaluationResult}
import rescala.graph.ReevaluationResult.{Dynamic, Static}
import rescala.pipelining.PipelineSpores
import rescala.propagation._

trait PropagateChangesOnly {
  
  self : PipelinePropagationImpl =>

  type S = PipelineSpores.type

   override protected def calculateQueueAction(head : Reactive[S], result : ReevaluationResult[S]) : (Boolean, Int, QueueAction) =
     result match {
      case Static(true) =>
        (true, -1, EnqueueDependencies)
      case Static(false) =>
        (false, -1, DoNothing)
      case Dynamic(hasChanged, diff) =>
        diff.removed foreach drop(head)
        diff.added foreach discover(head)
        val newLevel = maximumLevel(diff.novel) + 1
        if (head.bud.level < newLevel)
          (hasChanged, newLevel, RequeueReactive)
        else if (hasChanged)
          (true, newLevel, EnqueueDependencies)
        else (false, newLevel, DoNothing)
    }
   
   def propagationPhase(): Unit = levelQueue.evaluateQueue(evaluate)


}