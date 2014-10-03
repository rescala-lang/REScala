package rescala.propagation

import rescala.{Reactive, Pulsing}

sealed trait EvaluationResult

object EvaluationResult {
  case class Done(changed: Boolean, dependants: Set[Reactive], newDependencies: Set[Pulsing[Any]] = Set()) extends EvaluationResult
  case class Retry(dependencies: Set[Pulsing[_]]) extends EvaluationResult
}
