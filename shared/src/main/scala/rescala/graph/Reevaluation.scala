package rescala.graph

import rescala.propagation.Turn


sealed trait ReevaluationResult[S <: Struct]

object ReevaluationResult {
  case class Static[S <: Struct](changed: Boolean) extends ReevaluationResult[S]
  case class Dynamic[S <: Struct](changed: Boolean, diff: DepDiff[S]) extends ReevaluationResult[S]
}

case class DepDiff[S <: Struct](novel: Set[Reactive[S]], old: Set[Reactive[S]]) {
  lazy val added = novel.diff(old)
  lazy val removed = old.diff(novel)
}

/** reevaluation strategy for static dependencies */
trait StaticReevaluation[+P, S <: Struct] {
  this: Pulsing[P, S] =>

  /** side effect free calculation of the new pulse for the current turn */
  protected[rescala] def calculatePulse()(implicit turn: Turn[S]): Pulse[P]

  final override protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S] = {
    val p = calculatePulse()
    pulses.set(p)
    ReevaluationResult.Static(p.isChange)
  }
}


/** reevaluation strategy for dynamic dependencies */
trait DynamicReevaluation[+P, S <: Struct] {
  this: Pulsing[P, S] =>

  /** side effect free calculation of the new pulse and the new dependencies for the current turn */
  def calculatePulseDependencies(implicit turn: Turn[S]): (Pulse[P], Set[Reactive[S]])

  final override protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S] = {
    val (newPulse, newDependencies) = calculatePulseDependencies

    val oldDependencies = incoming
    if (newDependencies != oldDependencies) bud.updateIncoming(newDependencies)
    pulses.set(newPulse)
    ReevaluationResult.Dynamic(newPulse.isChange, DepDiff(newDependencies, oldDependencies))

  }
}
