package rescala.benchmarks.measureSizes

import org.openjdk.jol.info.GraphLayout
import rescala.core.{AccessHandler, ReSource, ReevTicket}
import rescala.default
import rescala.parrp.ParRPDefault
import rescala.scheduler.LevelbasedVariants

object ObjectSizes {

  def measure(name: String, roots: Any*): Unit = {
    println(s"======= $name")
    println(GraphLayout.parseInstance(roots*).toFootprint)
  }

  def main(args: Array[String]): Unit = {
    measure("var empty", rescala.default.Var.empty)
    measure("var 5", rescala.default.Var(5))
    measure("default empty signal", rescala.default.Signal {})
    measure("default empty signal x 10", List.fill(100)(rescala.default.Signal {}))
    measure(
      "synchron empty signal",
      rescala.interfaces.synchron.Signal(using rescala.interfaces.synchron.implicitScheduler) {}
    )

    def ptx = new ParRPDefault.ParRPTransaction(new rescala.parrp.Backoff(), None)
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
