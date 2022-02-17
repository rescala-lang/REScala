package tests.rescala.testtools

import rescala.Schedulers._
import rescala.fullmv.FullMVApi
import rescala.interface.RescalaInterface

import scala.concurrent.duration.DurationInt

object TestEngines {
  val fullMV = new FullMVApi(10.milliseconds, "generic FullMV Test API")

  val all: List[RescalaInterface] = List(synchron, parrp, toposort /*, fullMV */, sidup)
}
