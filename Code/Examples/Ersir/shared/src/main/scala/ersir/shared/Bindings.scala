package ersir.shared

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromString, writeToString}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import loci.MessageBuffer
import loci.registry.Binding
import loci.transmitter.Serializable
import loci.transmitter.transmittable.IdenticallyTransmittable
import rescala.extra.lattices.delta.CContext._
import rescala.extra.lattices.delta.crdt.RRGA
import rescala.extra.lattices.delta.crdt.RRGA._

import scala.concurrent.Future
import scala.util.Try

case class Posting(title: String, desc: String, img: String)

object Posting {
  def parse(textfield: String, url: String = ""): Posting = {
    val split              = textfield.split("\n", 2)
    val Array(title, desc) = if (split.length == 2) split else Array(textfield, "")
    Posting(title, desc, url)
  }

  implicit val PostingCodec: JsonValueCodec[Posting] = JsonCodecMaker.make
}

object Bindings {
  implicit def jsoniterBasedSerializable[T](implicit codec: JsonValueCodec[T]): Serializable[T] = new Serializable[T] {
    override def serialize(value: T): MessageBuffer = MessageBuffer.encodeString(writeToString(value))

    override def deserialize(value: MessageBuffer): Try[T] = Try(readFromString[T](value.decodeString))
  }

  implicit val _Tbm: IdenticallyTransmittable[RRGA.State[Posting, DietMapCContext]] = IdenticallyTransmittable()

  val crdtDescriptions
      : Binding[RRGA.State[Posting, DietMapCContext] => Unit, RRGA.State[Posting, DietMapCContext] => Future[Unit]] =
    Binding[RRGA.State[Posting, DietMapCContext] => Unit]("postings"): @scala.annotation.nowarn
}
