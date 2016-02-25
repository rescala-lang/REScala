package rescala.pipelining.propagation

import rescala.graph.ReevaluationResult.{Dynamic, Static}
import rescala.graph.{Reactive, ReevaluationResult}
import rescala.pipelining.PipelineStruct

private[pipelining] trait PropagateNoChanges {

  self : PipelinePropagationImpl =>

  type S = PipelineStruct.type

   protected def evaluateNoChange(head : Reactive[S]) : QueueAction

   override protected def calculateQueueAction(head : Reactive[S], result : ReevaluationResult[S]) = {
     result match {
      case Static(hasChanged) =>
        (hasChanged, -1, EnqueueDependencies)
      case Dynamic(hasChanged, diff) =>
        diff.removed foreach drop(head)
        diff.added foreach discover(head)
        val newLevel = maximumLevel(diff.novel) + 1
        val action = if (head.bud.level < newLevel) RequeueReactive else EnqueueDependencies
        (hasChanged, newLevel,  action)
    }
   }
   
   def propagationPhase(): Unit = levelQueue.evaluateQueue(evaluate, evaluateNoChange)


}