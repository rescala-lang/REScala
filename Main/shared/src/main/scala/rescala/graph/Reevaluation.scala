package rescala.graph

import rescala.engine.{Engine, Turn}
import rescala.graph.Pulse.NoChange

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
  case class Static[A](isChange: Boolean, value: A) extends ReevaluationResult[A, Nothing]
  def Static[P](value: Pulse[P]): ReevaluationResult[Pulse[P], Nothing] =  Static(value.isChange, value)

  /**
    * Result of the dynamic re-evaluation of a reactive value.
    * When using a dynamic dependency model, the dependencies of a value may change at runtime if it is re-evaluated
    */
  case class Dynamic[A, S <: Struct](isChange: Boolean, value: A, dependencies: Set[Reactive[S]]) extends ReevaluationResult[A, S] {
    def depDiff(oldDependencies: Set[Reactive[S]]): DepDiff[S] = new DepDiff(dependencies, oldDependencies)
  }
  def Dynamic[P, S <: Struct](value: Pulse[P], dependencies: Set[Reactive[S]]): ReevaluationResult[Pulse[P], S] =  Dynamic(value.isChange, value, dependencies)

  /**
    * Calculates and stores added or removed dependencies of a reactive value.
    *
    * @param novel Set of dependencies after re-evaluation
    * @param old   Set of dependencies before re-evaluation
    * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
    */
  class DepDiff[S <: Struct] private[ReevaluationResult] (val novel: Set[Reactive[S]], val old: Set[Reactive[S]]) {
    val added: Set[Reactive[S]] = novel.diff(old)
    val removed: Set[Reactive[S]] = old.diff(novel)
  }
}


trait Disconnectable[S <: Struct] extends Reactive[S] {
  override type Value >: Pulse[Nothing]
  @volatile private var disconnected = false

  final def disconnect()(implicit engine: Engine[S, Turn[S]]): Unit = {
    engine.transaction(this) { turn =>
      disconnected = true
    }
  }


  abstract final override protected[rescala] def reevaluate(ticket: Turn[S]): ReevaluationResult[Value, S] = {
    if (disconnected) {
      ReevaluationResult.Dynamic(NoChange, Set.empty[Reactive[S]])
    }
    else {
      super.reevaluate(ticket)
    }
  }

}
