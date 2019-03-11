package ersir.shared

import io.circe.generic.auto._
import loci.registry.Binding
import loci.serializer.circe._
import rescala.distributables.DistributedSignal._
import rescala.distributables.PGrowOnlyLog

case class Emergentcy(title: String, desc: String, img: String)

object Bindings {
  val crdtDescriptions = Binding[PGrowOnlyLog[Emergentcy]]("crdtDescriptions")
}
