package rescala.propagation

import rescala.Dependant

sealed trait EvaluationResult

object EvaluationResult {
  case class Dependants(dependants: Set[Dependant]) extends EvaluationResult
  case object Retry extends EvaluationResult
}
