package rescala.core

/**
  * Indicator for the result of a re-evaluation of a reactive value.
  */
class ReevaluationResult[+T, S <: Struct] private(
  val valueChanged: Boolean,
  val indepsChanged: Boolean,
  val indepsAfter: Set[ReSource[S]],
  val value: T,
  indepsAdded: Set[ReSource[S]], indepsRemoved: Set[ReSource[S]]
) {

  def commitDependencyDiff(turn: ReevaluationStateAccess[S], node: Reactive[S]): Unit = {
    if(indepsChanged) {
      indepsRemoved.foreach(turn.drop(_, node))
      indepsAdded.foreach(turn.discover(_, node))
      turn.writeIndeps(node, indepsAfter)
    }
  }

}

object ReevaluationResult {

  /**
    * Result of the static re-evaluation of a reactive value.
    */
  def StaticPulse[P, S <: Struct](value: Pulse[P], unchangedIndeps: Set[ReSource[S]]): ReevaluationResult[Pulse[P], S] =
    Static(value, value.isChange, unchangedIndeps)

  def Static[P, S <: Struct](value: P, propagate: Boolean, unchangedIndeps: Set[ReSource[S]]): ReevaluationResult[P, S] =
    new ReevaluationResult(value = value, valueChanged = propagate, indepsChanged = false, indepsAfter = unchangedIndeps, indepsAdded = Set.empty, indepsRemoved = Set.empty)



  /**
    * Result of the dynamic re-evaluation of a reactive value.
    * When using a dynamic dependency model, the dependencies of a value may change at runtime if it is re-evaluated
    */
  def DynamicPulse[P, S <: Struct]
  (value: Pulse[P], indepsAfter: Set[ReSource[S]], indepsAdded: Set[ReSource[S]], indepsRemoved: Set[ReSource[S]]): ReevaluationResult[Pulse[P], S] =
    Dynamic(value, value.isChange, indepsAfter, indepsAdded, indepsRemoved)

  def Dynamic[P, S <: Struct]
  (value: P, propagate: Boolean, indepsAfter: Set[ReSource[S]], indepsAdded: Set[ReSource[S]], indepsRemoved: Set[ReSource[S]]): ReevaluationResult[P, S] =
    new ReevaluationResult(value = value, valueChanged = propagate, indepsChanged = indepsAdded.nonEmpty || indepsRemoved.nonEmpty,
      indepsAfter = indepsAfter, indepsAdded = indepsAdded, indepsRemoved = indepsRemoved)
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


  abstract final override protected[rescala] def reevaluate(turn: Turn[S], before: Value, indeps: Set[ReSource[S]]): ReevaluationResult[Value, S] = {
    if (disconnected) {
      ReevaluationResult.Dynamic[Value, S](before, propagate = false, indepsAfter = Set.empty, indepsAdded = Set.empty, indepsRemoved = indeps)
    }
    else {
      super.reevaluate(turn, before, indeps)
    }
  }

}
