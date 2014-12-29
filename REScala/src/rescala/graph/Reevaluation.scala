package rescala.graph

import rescala.turns.Turn


sealed trait ReevaluationResult

object ReevaluationResult {
  case class Static(changed: Boolean) extends ReevaluationResult
  case class Dynamic(changed: Boolean, diff: DepDiff) extends ReevaluationResult
}

case class DepDiff(novel: Set[Reactive], old: Set[Reactive]) {
  lazy val added = novel.diff(old)
  lazy val removed = old.diff(novel)
}

/** reevaluation strategy for static dependencies */
trait StaticReevaluation[+P] {
  this: Pulsing[P] =>
  /** side effect free calculation of the new pulse for the current turn */
  protected[rescala] def calculatePulse()(implicit turn: Turn): Pulse[P]

  final override protected[rescala] def reevaluate()(implicit turn: Turn): ReevaluationResult = {
    val p = calculatePulse()
    pulses.set(p)
    ReevaluationResult.Static(p.isChange)
  }
}


/** reevaluation strategy for dynamic dependencies */
trait DynamicReevaluation[+P] {
  this: Pulsing[P] =>

  private val dependencies: Buffer[Set[Reactive]] = engine.buffer(Set(), (_, x) => x, lock)

  /** side effect free calculation of the new pulse and the new dependencies for the current turn */
  def calculatePulseDependencies(implicit turn: Turn): (Pulse[P], Set[Reactive])

  final override protected[rescala] def reevaluate()(implicit turn: Turn): ReevaluationResult = {
    val (newPulse, newDependencies) = calculatePulseDependencies

    val oldDependencies = dependencies.get
    dependencies.set(newDependencies)
    pulses.set(newPulse)
    ReevaluationResult.Dynamic(newPulse.isChange, DepDiff(newDependencies, oldDependencies))

  }
}
