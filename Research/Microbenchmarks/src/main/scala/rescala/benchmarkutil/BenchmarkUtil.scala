package rescala.benchmarkutil

import rescala.core.{Struct, Turn}
import rescala.reactives.Signal

object BenchmarkUtil {
  def directGet[A, S <: Struct](source: Signal[A, S], t: Turn[S]): A = t.makeStaticReevaluationTicket().staticDepend(source)
}
