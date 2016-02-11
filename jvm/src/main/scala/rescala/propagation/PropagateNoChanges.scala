package rescala.propagation

import rescala.graph.Reactive
import rescala.graph.ReevaluationResult
import rescala.graph.ReevaluationResult.{Static, Dynamic}
import rescala.pipelining.PipelineSpores

trait PropagateNoChanges {

  self : PropagationImpl[PipelineSpores.type] =>

  type S = PipelineSpores.type

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