package rescala.propagation

sealed abstract class QueueAction
case object EnqueueDependencies extends QueueAction
case object RequeueReactive extends QueueAction
case object DoNothing extends QueueAction
  