package rescala.core


trait Result[T, S <: Struct] {
  def propagate: Boolean
  def forValue(f: T => Unit): Unit
  def forEffect(f: Observation => Unit): Unit
  def getDependencies(): Option[Set[ReSource[S]]]
}

trait Observation {
  def execute(): Unit
}

trait Disconnectable[S <: Struct] {
  def disconnect()(implicit engine: Scheduler[S]): Unit
}


trait DisconnectableImpl[S <: Struct] extends Derived[S] with Disconnectable[S] {
  @volatile private var disconnected = false
  final def disconnect()(implicit engine: Scheduler[S]): Unit = {
    disconnected = true
  }

  def guardReevaluate(rein: ReIn)(normalEval: => Rout): Rout = {
    if (disconnected) {
      rein.trackDependencies(Set.empty)
      rein
    }
    else {
      normalEval
    }
  }

}
