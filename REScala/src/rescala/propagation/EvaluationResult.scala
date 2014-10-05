package rescala.propagation

sealed trait EvaluationResult

object EvaluationResult {
  case class Done(changed: Boolean, newDependencies: Set[Reactive] = Set()) extends EvaluationResult
  case class Retry(dependencies: Set[Reactive]) extends EvaluationResult
}
