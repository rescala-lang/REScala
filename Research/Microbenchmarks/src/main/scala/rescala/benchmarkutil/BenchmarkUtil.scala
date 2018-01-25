package rescala.benchmarkutil

import rescala.core.{Struct, TurnImpl}
import rescala.reactives.Signal

object BenchmarkUtil {
  def directGet[A, S <: Struct](source: Signal[A, S], t: TurnImpl[S]): A = t.makeStaticReevaluationTicket().staticDepend(source)
}
