package rescala.graph

import rescala.turns.Turn


sealed trait ReevaluationResult[S <: Spores]

object ReevaluationResult {
  case class Static[S <: Spores](changed: Boolean) extends ReevaluationResult[S]
  case class Dynamic[S <: Spores](changed: Boolean, diff: DepDiff[S]) extends ReevaluationResult[S]
}

case class DepDiff[S <: Spores](novel: Set[Reactive[S]], old: Set[Reactive[S]]) {
  lazy val added = novel.diff(old)
  lazy val removed = old.diff(novel)
}

/** reevaluation strategy for static dependencies */
trait StaticReevaluation[+P, S <: Spores] {
  this: Pulsing[P, S] =>

  protected def staticIncoming: Set[Reactive[S]]
  override protected[rescala] def incoming(implicit turn: Turn[S]): Set[Reactive[S]] = staticIncoming

  /** side effect free calculation of the new pulse for the current turn */
  protected[rescala] def calculatePulse()(implicit turn: Turn[S]): Pulse[P]

  final override protected[rescala] def reevaluate()(implicit turn: Turn[S]): ReevaluationResult[S] = {
    val p = calculatePulse()
    pulses.set(p)
    ReevaluationResult.Static(p.isChange)
  }
}


/** reevaluation strategy for dynamic dependencies */
trait DynamicReevaluation[+P, S <: Spores] {
  this: Pulsing[P, S] =>

  override protected[rescala] def incoming(implicit turn: Turn[S]): Set[Reactive[S]] = bud.incoming

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
