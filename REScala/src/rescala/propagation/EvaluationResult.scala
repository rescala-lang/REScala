package rescala.propagation

import rescala.{Dependency, Dependant}

sealed trait EvaluationResult

object EvaluationResult {
  case class Done(dependants: Set[Dependant]) extends EvaluationResult
  case class Retry(dependencies: Set[Dependency[_]]) extends EvaluationResult
}
