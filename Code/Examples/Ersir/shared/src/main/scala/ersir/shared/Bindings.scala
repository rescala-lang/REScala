package ersir.shared

import io.circe.generic.auto._
import io.circe.generic.semiauto._
import io.circe.{Decoder, Encoder}
import loci.registry.{Binding, BindingBuilder}
import loci.serializer.circe._
import loci.transmitter.IdenticallyTransmittable
import rescala.extra.lattices.sequences.RGOA.RGOA

case class Posting(title: String,
                   desc: String,
                   img: String)

object Posting {
  def parse(textfield: String, url: String = ""): Posting = {
    val split = textfield.split("\n", 2)
    val Array(title, desc) = if (split.length == 2) split else Array(textfield, "")
    Posting(title, desc, url)
  }

  implicit val postingEncoder: Encoder[Posting] = deriveEncoder[Posting]
  implicit val postingDecoder: Decoder[Posting] = deriveDecoder[Posting]

}

object Bindings {
  implicit def rgoaEncoder[A: Encoder]: Encoder[RGOA[A]] = deriveEncoder[RGOA[A]]
  implicit def rgoaDecoder[A: Decoder]: Decoder[RGOA[A]] = deriveDecoder[RGOA[A]]


  implicit def epocheEncoder[A: Encoder]: Encoder[Epoche[A]] = deriveEncoder
  implicit def epocheDecoder[A: Decoder]: Decoder[Epoche[A]] = deriveDecoder

  implicit val _Tbm: IdenticallyTransmittable[Epoche[RGOA[Posting]]] = IdenticallyTransmittable()


  val crdtDescriptions = Binding[Epoche[RGOA[Posting]] => Unit]("postings")
}
