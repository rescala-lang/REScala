package rescala.pipelining.propagation

private[pipelining] sealed abstract class QueueAction
private[pipelining] case object EnqueueDependencies extends QueueAction
private[pipelining] case object RequeueReactive extends QueueAction
private[pipelining] case object DoNothing extends QueueAction
  