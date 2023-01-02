package viscel.shared

import loci.registry.Binding
import loci.serializer.jsoniterScala._
import loci.transmitter.IdenticallyTransmittable
import viscel.shared.JsoniterCodecs._

object Bindings {
  type IT[V] = IdenticallyTransmittable[V]
  val version = Binding[String]("version")
}
