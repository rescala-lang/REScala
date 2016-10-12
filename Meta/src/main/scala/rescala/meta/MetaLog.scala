package rescala.meta

trait MetaLog[T] {
  protected[meta] val node : DataFlowRef[T]
}

case class LoggedCreate[T](override val node : DataFlowRef[T]) extends MetaLog[T]
case class LoggedDisconnect[T](override val node : DataFlowRef[T]) extends MetaLog[T]
case class LoggedSet[T](override val node : VarRef[T], value : T) extends MetaLog[T]
case class LoggedFire[T](override val node : EvtRef[T], value : T) extends MetaLog[T]