package rescala.propagation

sealed trait EvaluationResult

object EvaluationResult {
  case class Done(changed: Boolean, changedDependencies: Option[DependencyDiff] = None) extends EvaluationResult
  case class DependencyDiff(newDependencies: Set[Reactive], oldDependencies: Set[Reactive]) extends EvaluationResult
}
