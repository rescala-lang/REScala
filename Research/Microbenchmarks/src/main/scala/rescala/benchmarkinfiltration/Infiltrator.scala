package rescala.benchmarkinfiltration

import rescala.core.Struct
import rescala.reactives.Signal
import rescala.sharedimpl.TurnImpl

/** object contained in rescala package has access to internal methods */
object Infiltrator {
  def directGet[A, S <: Struct](source: Signal[A, S], t: TurnImpl[S]): A = t.makeStaticReevaluationTicket().staticDepend(source)
}
