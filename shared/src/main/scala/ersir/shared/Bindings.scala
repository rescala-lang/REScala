package ersir.shared

import io.circe.generic.auto._
import loci.registry.Binding
import loci.serializer.circe._
import rescala.crdts.distributables.DistributedSignal._

case class Emergentcy(title: String, desc: String, img: String)

object Bindings {
  val crdtDescriptions = Binding[rescala.crdts.distributables.PGrowOnlyLog[Emergentcy]]("crdtDescriptions")
}
