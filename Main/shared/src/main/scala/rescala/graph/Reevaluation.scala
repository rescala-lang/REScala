package rescala.graph

import rescala.engine.Engine
import rescala.propagation.Turn

/**
  * Indicator for the result of a re-evaluation of a reactive value.
  */
sealed trait ReevaluationResult[A, S <: Struct]

object ReevaluationResult {

  /**
    * Result of the static re-evaluation of a reactive value.
    */
  case class Static[A, S <: Struct](value: Option[A]) extends ReevaluationResult[A, S]
  def Static[P, S <: Struct](value: Pulse[P]): ReevaluationResult[Pulse[P], S] =  Static(if (value.isChange) Some(value) else None)

  /**
    * Result of the dynamic re-evaluation of a reactive value.
    * When using a dynamic dependency model, the dependencies of a value may change at runtime if it is re-evaluated
    */
  case class Dynamic[A, S <: Struct](value: Option[A], dependencies: Set[Reactive[S]]) extends ReevaluationResult[A, S]
  def Dynamic[P, S <: Struct](value: Pulse[P], dependencies: Set[Reactive[S]]): ReevaluationResult[Pulse[P], S] =  Dynamic(if (value.isChange) Some(value) else None, dependencies)

}

/**
  * Calculates and stores added or removed dependencies of a reactive value.
  *
  * @param novel Set of dependencies after re-evaluation
  * @param old   Set of dependencies before re-evaluation
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
case class DepDiff[S <: Struct](novel: Set[Reactive[S]], old: Set[Reactive[S]]) {
  lazy val added: Set[Reactive[S]] = novel.diff(old)
  lazy val removed: Set[Reactive[S]] = old.diff(novel)
}

trait Disconnectable[S <: Struct] extends Reactive[S] {

  @volatile private var disconnected = false

  final def disconnect()(implicit engine: Engine[S, Turn[S]]): Unit = {
    engine.plan(this) { turn =>
      disconnected = true
    }
  }


  abstract final override protected[rescala] def reevaluate(ticket: S#Ticket[S]): ReevaluationResult[Value, S] = {
    if (disconnected) {
      ReevaluationResult.Dynamic(None, Set.empty)
    }
    else {
      super.reevaluate(ticket)
    }
  }

}
