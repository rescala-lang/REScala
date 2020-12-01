package rescala.core

trait Result[T, S <: Struct] {

  /** True iff outputs must also be reevaluated, false iff the propagation ends here. */
  def activate: Boolean

  /** No-allocation accessor for the optional new value. */
  def forValue(f: T => Unit): Unit

  /** No-allocation accessor for the effect caused by the reevaluation. */
  def forEffect(f: Observation => Unit): Unit

  /** New input resources.
    * None for static reactives.
    * Otherwise a list of all static reactives, and accessed dynamic reactives.
    */
  def inputs(): Option[Set[ReSource[S]]]
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
    } else {
      normalEval
    }
  }

}
