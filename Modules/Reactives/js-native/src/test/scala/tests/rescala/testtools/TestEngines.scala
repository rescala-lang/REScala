package tests.rescala.testtools

import rescala.scheduler.Schedulers._

object TestEngines {
  val all = Seq(toposort, synchron, sidup)
}
