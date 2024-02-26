package tests.rescala.testtools

import reactives.interfaces.*
import reactives.fullmv.FullMVApi
import reactives.operator.Interface

import scala.concurrent.duration.DurationInt

object TestEngines {
  val fullMV = new FullMVApi(10.milliseconds, "generic FullMV Test API")

  val all: List[Interface] = List(synchron/*,  parrp, toposort*/ /*, fullMV */ /*, sidup*/)
}
