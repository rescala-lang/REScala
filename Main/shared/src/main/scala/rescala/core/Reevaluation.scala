package rescala.core


sealed trait ReevaluationResult[+T, S] {
  val propagate: Boolean
  def forValue(f: T => Unit): Unit = ()
  def forEffect(f: (() => Unit) => Unit): Unit = ()
}

/**
  * Indicator for the result of a re-evaluation of a reactive value.
  */
case class ReevaluationResultWithValue[+T, S <: Struct] private(value: T, propagate: Boolean) extends ReevaluationResult[T, S] {
  override def forValue(f: T => Unit): Unit = f(value)
}

case class ReevaluationResultWithoutValue[S <: Struct] (propagate: Boolean) extends ReevaluationResult[Nothing, S]

case class ReevaluationResultEffect[S <: Struct] (effect: () => Unit, propagate: Boolean) extends ReevaluationResult[Nothing, S] {
  override def forEffect(f: (() => Unit) => Unit): Unit = f(effect)
}

object ReevaluationResultWithValue {

  /**
    * Result of the static re-evaluation of a reactive value.
    */
  def StaticPulse[P, S <: Struct](value: Pulse[P]): ReevaluationResult[Pulse[P], S] = {
    if (value.isChange) Static(value = value, propagate = value.isChange)
    else ReevaluationResultWithoutValue(propagate = false)
  }

  def Static[P, S <: Struct](value: P, propagate: Boolean): ReevaluationResult[P, S] =
    new ReevaluationResultWithValue(value = value, propagate = propagate)



  /**
    * Result of the dynamic re-evaluation of a reactive value.
    * When using a dynamic dependency model, the dependencies of a value may change at runtime if it is re-evaluated
    */
  def DynamicPulse[P, S <: Struct](value: Pulse[P]): ReevaluationResult[Pulse[P], S] = {
    if (value.isChange) new ReevaluationResultWithValue(value = value, propagate = value.isChange)
    else ReevaluationResultWithoutValue(propagate = false)
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


  abstract final override protected[rescala] def reevaluate(turn: DynamicTicket[S], before: Value): ReevaluationResult[Value, S] = {
    if (disconnected) {
      ReevaluationResultWithoutValue[S](propagate = false)
    }
    else {
      super.reevaluate(turn, before)
    }
  }

}
