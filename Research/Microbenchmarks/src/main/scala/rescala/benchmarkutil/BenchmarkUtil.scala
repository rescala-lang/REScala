package rescala.benchmarkutil

import rescala.engine.Turn
import rescala.graph.Struct
import rescala.reactives.Signal

object BenchmarkUtil {
  def directGet[A, S <: Struct](source: Signal[A, S], t: Turn[S]): A = t.after(source).get
}
