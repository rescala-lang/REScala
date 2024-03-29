package reactives.extra.precondition

import reactives.core.{AdmissionTicket, DynamicTicket, ReSource}
import reactives.operator.Interface.State

class Precondition[T](val accessed: List[ReSource.of[State]], fun: DynamicTicket[State] => T) {
  def check(using at: AdmissionTicket[State]): T =
    fun(at.tx.preconditionTicket)
}

object Precondition {

  inline def prepare[T](inline expr: T) = {
    val (sources, fun, isStatic) =
      reactives.macros.MacroLegos.getDependencies[T, ReSource.of[State], reactives.core.DynamicTicket[State], false](
        expr
      )
    Precondition(sources, fun)

  }
}
