package ersir.shared

import io.circe.generic.auto._
import io.circe.generic.semiauto._
import io.circe.{Decoder, Encoder}
import loci.registry.Binding
import loci.serializer.circe._
import rescala.lattices.sequences.RGOA.RGOA

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
  val crdtDescriptions = Binding[Epoche[RGOA[Posting]]]("postings")
}
