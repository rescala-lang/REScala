package rescala.core


trait Result[+T, S <: Struct] {
  def propagate: Boolean
  def forValue(f: T => Unit): Unit
  def forEffect(f: (() => Unit) => Unit): Unit
  def getDependencies(): Option[Set[ReSource[S]]]
}


object Result {
  def fromPulse[P, S <: Struct](t: ReevTicket[Pulse[P], S], value: Pulse[P]): Result[Pulse[P], S] = {
    if (value.isChange) t.withValue(value)
    else t
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


  abstract final override protected[rescala] def reevaluate(turn: ReevTicket[Value, S], before: Value): Result[Value, S] = {
    if (disconnected) {
      turn.trackDependencies()
      turn
    }
    else {
      super.reevaluate(turn, before)
    }
  }

}
