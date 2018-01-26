package rescala.core


sealed trait Result[+T, S] {
  val propagate: Boolean
  def forValue(f: T => Unit): Unit = ()
  def forEffect(f: (() => Unit) => Unit): Unit = ()
}


object Result {

  case class WithValue[+T, S <: Struct](value: T, propagate: Boolean) extends Result[T, S] {
    override def forValue(f: T => Unit): Unit = f(value)
  }

  case class NoValue[S <: Struct](propagate: Boolean) extends Result[Nothing, S]

  case class Effect[S <: Struct](effect: () => Unit, propagate: Boolean) extends Result[Nothing, S] {
    override def forEffect(f: (() => Unit) => Unit): Unit = f(effect)
  }

  /**
    * Result of the static re-evaluation of a reactive value.
    */
  def fromPulse[P, S <: Struct](value: Pulse[P]): Result[Pulse[P], S] = {
    if (value.isChange) Static(value = value, propagate = value.isChange)
    else NoValue(propagate = false)
  }

  def Static[T, S <: Struct](value: T, propagate: Boolean): Result[T, S] =
    new WithValue[T, S](value, propagate)


  /**
    * Result of the dynamic re-evaluation of a reactive value.
    * When using a dynamic dependency model, the dependencies of a value may change at runtime if it is re-evaluated
    */
  def dynamicFromPulse[P, S <: Struct](value: Pulse[P]): Result[Pulse[P], S] = {
    if (value.isChange) WithValue(value = value, propagate = value.isChange)
    else NoValue(propagate = false)
  }

}

trait Disconnectable[S <: Struct] {
  def disconnect()(implicit engine: Scheduler[S]): Unit
}


trait DisconnectableImpl[S <: Struct] extends Reactive[S] with Disconnectable[S] {
  @volatile private var disconnected = false
  final def disconnect()(implicit engine: Scheduler[S]): Unit = {
    engine.transaction(this) { turn =>
      disconnected = true
    }
  }


  abstract final override protected[rescala] def reevaluate(turn: ReevTicket[S], before: Value): Result[Value, S] = {
    if (disconnected) {
      turn.trackDependencies()
      Result.NoValue[S](propagate = false)
    }
    else {
      super.reevaluate(turn, before)
    }
  }

}
