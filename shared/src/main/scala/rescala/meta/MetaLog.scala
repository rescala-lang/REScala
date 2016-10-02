package rescala.meta

trait MetaLog[T] {
  protected[meta] val node : ReactiveNode[T]
}

case class LoggedSet[T](override val node : ReactiveNode[T], value : T) extends MetaLog[T]
case class LoggedFire[T](override val node : ReactiveNode[T], value : T) extends MetaLog[T]