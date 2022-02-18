package benchmarks.measureSizes

import kofre.causality.impl.ArrayRanges
import org.openjdk.jol.info.GraphLayout
import rescala.default

object ObjectSizes {

  def measure(name: String, roots: Any*): Unit = {
    println(s"======= $name")
    println(GraphLayout.parseInstance(roots: _*).toFootprint)
  }

  def main(args: Array[String]): Unit = {
    measure("empty range", ArrayRanges.empty)
    measure("6 elem range", ArrayRanges.empty.add(1).add(2).add(3).add(4).add(5).add(6))
    measure("var empty", rescala.default.Var.empty)
    measure("var 5", rescala.default.Var(5))
    measure("default empty signal", rescala.default.Signal {})
    measure("default empty signal x 10", List.fill(100)(rescala.default.Signal {}): _*)
    measure("synchron empty signal", rescala.Schedulers.synchron.Signal {})

    def tx = new default.ParRPTransaction(new rescala.parrp.Backoff(), None)
    measure("transaction", List.fill(100)(tx))
    measure(
      "reev ticket",
      new rescala.default.ReevTicket(tx, ()) {
        override protected def staticAccess(reactive: default.ReSource): reactive.Value  = ???
        override protected def dynamicAccess(reactive: default.ReSource): reactive.Value = ???
      }
    )
  }

}
