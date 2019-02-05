package ersir.shared

import io.circe.generic.auto._
import loci.registry.Binding
import loci.serializer.circe._
import rescala.crdts.pvars.Publishable._


object Bindings {
  val crdtDescriptions = Binding[rescala.crdts.pvars.PGrowOnlyLog[String]]("crdtDescriptions")
}
