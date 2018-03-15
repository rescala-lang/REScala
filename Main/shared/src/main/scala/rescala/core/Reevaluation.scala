package rescala.core


trait Result[T, S <: Struct] {
  def propagate: Boolean
  def forValue(f: T => Unit): Unit
  def forEffect(f: (() => Unit) => Unit): Unit
  def getDependencies(): Option[Set[ReSource[S]]]
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


  abstract final override protected[rescala] def reevaluate(rein: ReIn): Rout = {
    if (disconnected) {
      rein.trackDependencies(Set.empty)
      rein
    }
    else {
      super.reevaluate(rein)
    }
  }

}
