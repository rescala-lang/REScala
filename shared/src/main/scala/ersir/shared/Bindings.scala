package ersir.shared

import io.circe.generic.auto._
import io.circe.generic.extras.Configuration
import loci.registry.Binding
import loci.serializer.circe._
import rescala.distributables.DistributedSignal._
import rescala.distributables.PGrowOnlyLog

case class Posting(title: String,
                   desc: String,
                   img: String,
                   timestamp: Long)

object Posting {
  implicit val config: Configuration = Configuration.default.withDefaults
}

object Bindings {
  val crdtDescriptions = Binding[PGrowOnlyLog[Posting]]("crdtDescriptions")
}
