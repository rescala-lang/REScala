package reactives.meta

import reactives.core.{CreationTicket, Struct}

case class ObserverData[S <: Struct](
    onSuccess: Nothing => Unit,
    onFailure: Throwable => Unit,
    ticket: CreationTicket[S]
) {
  // Prevent structural equality check for case-classes
  private class Data
  private val _data = new Data()

  override def equals(obj: scala.Any): Boolean =
    obj match {
      case n: ObserverData[_] => n._data == _data
      case _                  => false
    }
  override def hashCode(): Int = _data.hashCode()
}

trait MetaLog[T] {
  protected[meta] val node: DataFlowRef[T]
}

case class LoggedCreate[T](override val node: DataFlowRef[T])     extends MetaLog[T]
case class LoggedDisconnect[T](override val node: DataFlowRef[T]) extends MetaLog[T]
case class LoggedSet[T](override val node: VarRef[T], value: T)   extends MetaLog[T]
case class LoggedFire[T](override val node: EvtRef[T], value: T)  extends MetaLog[T]
case class LoggedObserve[T, S <: Struct](override val node: DataFlowRef[T], observer: ObserverData[S])
    extends MetaLog[T]
