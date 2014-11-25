package rescala.propagation.turns.instances

import rescala.propagation.EvaluationResult.{Dynamic, Static}
import rescala.propagation.turns.Turn
import rescala.propagation.{DepDiff, Reactive}

object Evaluator {

  /** evaluates a single reactive */
  def evaluate(head: Reactive)(implicit turn: Turn): Result = {
    val result = head.reevaluate()
    result match {
      case Static(hasChanged) => Done(head, hasChanged, -42)
      case Dynamic(hasChanged, diff) =>
        val newLevel = maximumLevel(diff.novel) + 1
        if (head.level.get >= newLevel) {
          Done(head, hasChanged, newLevel, Some(diff))
        }
        else {
          Redo(head, newLevel, Some(diff))
        }
    }
  }

  def maximumLevel(dependencies: Set[Reactive])(implicit turn: Turn): Int = dependencies.foldLeft(-1)((acc, r) => math.max(acc, r.level.get))

  sealed trait Result {
    def head: Reactive
    def getDiff: Option[DepDiff]
    def requeue(q: Int => Reactive => Unit)(implicit turn: Turn): Unit
  }
  case class Done(head: Reactive, changed: Boolean, level: Int, getDiff: Option[DepDiff] = None) extends Result {
    override def requeue(q: Int => Reactive => Unit)(implicit turn: Turn): Unit = if (changed) head.dependants.get.foreach(q(level))
  }
  case class Redo(head: Reactive, level: Int, getDiff: Option[DepDiff]) extends Result {
    override def requeue(q: Int => Reactive => Unit)(implicit turn: Turn): Unit = q(level)(head)
  }


}

