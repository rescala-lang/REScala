package rescala.graph

import rescala.engines.Engine
import rescala.propagation.Turn

/**
  * Indicator for the result of a re-evaluation of a reactive value.
  */
sealed trait ReevaluationResult[S <: Struct]

object ReevaluationResult {

  /**
    * Result of the static re-evaluation of a reactive value.
    *
    * @param changed Indicates if the value of the reactive value has been changed and further re-evaluation of dependent
    *                values is necessary.
    * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
    */
  case class Static[S <: Struct](changed: Boolean) extends ReevaluationResult[S]

  /**
    * Result of the dynamic re-evaluation of a reactive value.
    * When using a dynamic dependency model, the dependencies of a value may change at runtime if it is re-evaluated
    *
    * @param changed Indicates if the value of the reactive value has been changed and further re-evaluation of dependent
    *                values is necessary.
    * @param diff    List of reactive values this value depends on that have been removed or added through the re-evaluation
    * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
    */
  case class Dynamic[S <: Struct](changed: Boolean, diff: DepDiff[S]) extends ReevaluationResult[S]
}

/**
  * Calculates and stores added or removed dependencies of a reactive value.
  *
  * @param novel Set of dependencies after re-evaluation
  * @param old   Set of dependencies before re-evaluation
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
case class DepDiff[S <: Struct](novel: Set[Reactive[S]], old: Set[Reactive[S]]) {
  lazy val added = novel.diff(old)
  lazy val removed = old.diff(novel)
}

/**
  * Implementation of static re-evaluation of a reactive value.
  * Only calculates the stored value of the pulse and compares it with the old value.
  *
  * @tparam P Value type stored by the reactive value and its pulse
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait StaticReevaluation[+P, S <: Struct] extends Disconnectable[S] {
  this: Pulsing[P, S] =>

  /** side effect free calculation of the new pulse for the current turn */
  protected[rescala] def calculatePulse()(implicit turn: Turn[S]): Pulse[P]

  final override protected[rescala] def computeReevaluationResult()(implicit turn: Turn[S]): ReevaluationResult[S] = {
    val p = calculatePulse()
    set(p)
    ReevaluationResult.Static(p.isChange)
  }


}


/**
  * Implementation of dynamic re-evaluation of a reactive value.
  * Calculates the pulse and new dependencies, compares them with the old value and dependencies and returns the result.
  *
  * @tparam P Value type stored by the reactive value and its pulse
  * @tparam S Struct type that defines the spore type used to manage the reactive evaluation
  */
trait DynamicReevaluation[+P, S <: Struct] extends Disconnectable[S] {
  this: Pulsing[P, S] =>

  /** side effect free calculation of the new pulse and the new dependencies for the current turn */
  def calculatePulseDependencies(implicit turn: Turn[S]): (Pulse[P], Set[Reactive[S]])

  final override protected[rescala] def computeReevaluationResult()(implicit turn: Turn[S]): ReevaluationResult[S] = {
    val (newPulse, newDependencies) = calculatePulseDependencies

    val oldDependencies = turn.incoming(bud)
    set(newPulse)
    ReevaluationResult.Dynamic(newPulse.isChange, DepDiff(newDependencies, oldDependencies))

  }
}

trait Disconnectable[S <: Struct] {
  this: Reactive[S] =>

  @volatile private var disconnected = false

  final def disconnect()(implicit engine: Engine[S, Turn[S]]): Unit = {
    engine.plan(this) { turn =>
      disconnected = true
    }
  }

  protected[rescala] def computeReevaluationResult()(implicit turn: Turn[S]): ReevaluationResult[S]

  final override protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S] = {
    if (disconnected) {
      ReevaluationResult.Dynamic(changed = false, DepDiff(novel = Set.empty, old = turn.incoming(bud)))
    }
    else {
      computeReevaluationResult()
    }
  }

}
