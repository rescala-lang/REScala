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
  def Static[P, S <: Struct](value: Pulse[P], unchangedIndeps: Set[ReSource[S]]): ReevaluationResult[Pulse[P], S] =
    new ReevaluationResult(value = value, valueChanged = value.isChange, indepsChanged = false, indepsAfter = unchangedIndeps, indepsAdded = Set.empty, indepsRemoved = Set.empty)

  /**
    * Result of the dynamic re-evaluation of a reactive value.
    * When using a dynamic dependency model, the dependencies of a value may change at runtime if it is re-evaluated
    */
  def Dynamic[P, S <: Struct]
  (value: Pulse[P], indepsAfter: Set[ReSource[S]], indepsAdded: Set[ReSource[S]], indepsRemoved: Set[ReSource[S]]): ReevaluationResult[Pulse[P], S] =
    new ReevaluationResult(value = value, valueChanged = value.isChange, indepsChanged = indepsAdded.nonEmpty || indepsRemoved.nonEmpty,
      indepsAfter = indepsAfter, indepsAdded = indepsAdded, indepsRemoved = indepsRemoved)
}


trait Disconnectable[S <: Struct] extends Reactive[S] {
  override type Value >: Pulse[Nothing]
  @volatile private var disconnected = false
  final def disconnect()(implicit engine: Engine[S]): Unit = {
    engine.transaction(this) { turn =>
      disconnected = true
    }
  }


  abstract final override protected[rescala] def reevaluate(turn: Turn[S], before: Value, indeps: Set[ReSource[S]]): ReevaluationResult[Value, S] = {
    if (disconnected) {
      ReevaluationResult.Dynamic[Nothing, S](Pulse.NoChange, Set.empty, Set.empty, indeps)
    }
    else {
      super.reevaluate(turn, before, indeps)
    }
  }

}
