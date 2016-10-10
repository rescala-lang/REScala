package rescala.meta

trait MetaLog[T] {
  protected[meta] val node : DataFlowNode[T]
}

case class LoggedCreate[T](override val node : DataFlowNode[T]) extends MetaLog[T]
case class LoggedDisconnect[T](override val node : DataFlowNode[T]) extends MetaLog[T]
case class LoggedSet[T](override val node : DataFlowNode[T], value : T) extends MetaLog[T]
case class LoggedFire[T](override val node : DataFlowNode[T], value : T) extends MetaLog[T]