package rescala.propagation.turns.instances

import rescala.propagation.EvaluationResult.{Dynamic, Static}
import rescala.propagation.turns.Turn
import rescala.propagation.{DepDiff, Reactive}

object Evaluator {

  /** evaluates a single reactive */
  def evaluate(head: Reactive)(implicit turn: Turn): Result = {
    val result = head.reevaluate()
    result match {
      case Static(hasChanged) => Result(head, hasChanged, -42, None, redo = false)
      case Dynamic(hasChanged, diff) =>
        val newLevel = maximumLevel(diff.novel) + 1
        if (head.level.get >= newLevel) {
          Result(head, hasChanged, newLevel, Some(diff), redo = false)
        }
        else {
          Result(head, hasChanged, newLevel, Some(diff), redo = true)
        }
    }
  }

  def maximumLevel(dependencies: Set[Reactive])(implicit turn: Turn): Int = dependencies.foldLeft(-1)((acc, r) => math.max(acc, r.level.get))

  case class Result(head: Reactive, changed: Boolean, level: Int, getDiff: Option[DepDiff], redo: Boolean) {
    def requeue(q: (Int, Boolean) => Reactive => Unit)(implicit turn: Turn): Unit =
      if (redo) q(level, changed)(head)
      else if (changed) head.dependants.get.foreach(q(level, changed))
  }

}

