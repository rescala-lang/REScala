package rescala.meta

trait MetaLog {
  protected[meta] val node : ReactiveNode
}

case class LoggedSet[T](override val node : ReactiveNode, value : T) extends MetaLog
case class LoggedFire[T](override val node : ReactiveNode, value : T) extends MetaLog