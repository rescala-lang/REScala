package rescala.propagation

import rescala.{Reactive, Dependency}

sealed trait EvaluationResult

object EvaluationResult {
  case class Done(changed: Boolean, dependants: Set[Reactive]) extends EvaluationResult
  case class Retry(dependencies: Set[Dependency[_]]) extends EvaluationResult
}
