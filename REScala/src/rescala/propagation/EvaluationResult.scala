package rescala.propagation

import rescala.Dependant

sealed trait EvaluationResult

object EvaluationResult {
  case class Done(dependants: Set[Dependant]) extends EvaluationResult
  case object Retry extends EvaluationResult
}
