package rescala.benchmarkutil

import rescala.graph.Struct
import rescala.propagation.Turn
import rescala.reactives.Signal

object BenchmarkUtil {
  def directGet[A, S <: Struct](source: Signal[A, S], t: Turn[S]): A = source.pulse(t.makeTicket()).get
}
