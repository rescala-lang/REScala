package rescala.graph

import rescala.engine.{Engine, Turn}

/**
  * Indicator for the result of a re-evaluation of a reactive value.
  */
sealed trait ReevaluationResult[+A, +S <: Struct]{
  val isChange: Boolean
  val value: A
}

object ReevaluationResult {

  /**
    * Result of the static re-evaluation of a reactive value.
    */
  case class Static[+A](isChange: Boolean, value: A) extends ReevaluationResult[A, Nothing]
  def Static[P](value: Pulse[P]): Static[Pulse[P]] =  Static(value.isChange, value)

  /**
    * Result of the dynamic re-evaluation of a reactive value.
    * When using a dynamic dependency model, the dependencies of a value may change at runtime if it is re-evaluated
    */
  case class Dynamic[A, S <: Struct](isChange: Boolean, value: A, indepsAfter: Set[Reactive[S]], indepsAdded: Set[Reactive[S]], indepsRemoved: Set[Reactive[S]]) extends ReevaluationResult[A, S]
  def Dynamic[P, S <: Struct](value: Pulse[P], indepsAfter: Set[Reactive[S]], indepsAdded: Set[Reactive[S]], indepsRemoved: Set[Reactive[S]]): Dynamic[Pulse[P], S] =  Dynamic(value.isChange, value, indepsAfter, indepsAdded, indepsRemoved)

  val staticNoChange: Static[Pulse[Nothing]] = Static(Pulse.NoChange)
}


trait Disconnectable[S <: Struct] extends Reactive[S] {
  override type Value >: Pulse[Nothing]
  @volatile private var disconnected = false

  final def disconnect()(implicit engine: Engine[S]): Unit = {
    engine.transaction(this) { turn =>
      disconnected = true
    }
  }


  abstract final override protected[rescala] def reevaluate(turn: Turn[S], before: Value, indeps: Set[Reactive[S]]): ReevaluationResult[Value, S] = {
    if (disconnected) {
      ReevaluationResult.Dynamic[Nothing, S](Pulse.NoChange, indepsAfter = Set.empty, indepsAdded = Set.empty, indepsRemoved = indeps)
    }
    else {
      super.reevaluate(turn, before, indeps)
    }
  }

}
