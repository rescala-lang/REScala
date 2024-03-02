package reactives.benchmarks.measureSizes

import rdts.time.ArrayRanges
import org.openjdk.jol.info.GraphLayout
import reactives.core.{AccessHandler, ReSource, ReevTicket}
import reactives.default
import reactives.parrp.ParRPDefault
import reactives.scheduler.LevelbasedVariants

object ObjectSizes {

  def measure(name: String, roots: Any*): Unit = {
    println(s"======= $name")
    println(GraphLayout.parseInstance(roots*).toFootprint)
  }

  def main(args: Array[String]): Unit = {
    measure("empty range", ArrayRanges.empty)
    measure("6 elem range", ArrayRanges.empty.add(1).add(2).add(3).add(4).add(5).add(6))
    measure("var empty", reactives.default.Var.empty)
    measure("var 5", reactives.default.Var(5))
    measure("default empty signal", reactives.default.Signal {})
    measure("default empty signal x 10", List.fill(100)(reactives.default.Signal {}))

    def ptx = new ParRPDefault.ParRPTransaction(new reactives.parrp.Backoff(), None)
    measure("transaction", List.fill(100)(ptx))
    measure(
      "reev ticket",
      new ReevTicket(
        ptx,
        (),
        new AccessHandler {
          override def staticAccess(reactive: ReSource.of[ParRPDefault.ParRPState]): reactive.Value  = ???
          override def dynamicAccess(reactive: ReSource.of[ParRPDefault.ParRPState]): reactive.Value = ???
        }
      )
    )

    def stx = new LevelbasedVariants.SimpleNoLock()
    measure("nolock transaction", List.fill(100)(stx))
    measure(
      "nolock reev ticket",
      new ReevTicket(
        stx,
        (),
        new AccessHandler {
          override def staticAccess(reactive: ReSource.of[LevelbasedVariants.LevelState]): reactive.Value  = ???
          override def dynamicAccess(reactive: ReSource.of[LevelbasedVariants.LevelState]): reactive.Value = ???
        }
      )
    )
  }

}
