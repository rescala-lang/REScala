package ersir.shared

import cats.collections.Diet
import cats.implicits._
import io.circe.generic.semiauto._
import io.circe.{Decoder, Encoder}
import loci.registry.Binding
import loci.serializer.circe._
import loci.transmitter.IdenticallyTransmittable
import rescala.extra.lattices.delta.{CContext, Causal, Dot, TimedVal}
import rescala.extra.lattices.delta.CContext.DietMapCContext
import rescala.extra.lattices.delta.DotStore.DotFun
import rescala.extra.lattices.delta.crdt.{Elem, GOList, GOListNode, RGANode, RRGA}

import scala.concurrent.Future

case class Posting(title: String, desc: String, img: String)

object Posting {
  def parse(textfield: String, url: String = ""): Posting = {
    val split              = textfield.split("\n", 2)
    val Array(title, desc) = if (split.length == 2) split else Array(textfield, "")
    Posting(title, desc, url)
  }

  implicit val postingEncoder: Encoder[Posting] = deriveEncoder[Posting]
  implicit val postingDecoder: Decoder[Posting] = deriveDecoder[Posting]

}

object Bindings {
  implicit val DotDecoder: Decoder[Dot] = deriveDecoder
  implicit val DotEncoder: Encoder[Dot] = deriveEncoder

  implicit val RangeDecoder: Decoder[cats.collections.Range[Int]] = deriveDecoder
  implicit val RangeEncoder: Encoder[cats.collections.Range[Int]] = deriveEncoder

  implicit val DietDecoder: Decoder[Diet[Int]] = Decoder.decodeList[cats.collections.Range[Int]].map {
    _.foldLeft(Diet.empty[Int]) {
      (d, r) => d.addRange(r)
    }
  }
  implicit val DietEncoder: Encoder[Diet[Int]] =
    Encoder.encodeList[cats.collections.Range[Int]].contramap(_.foldLeftRange(List.empty[cats.collections.Range[Int]]) {
      (l, r) => r :: l
    })

  implicit val GOListNodeDecoder: Decoder[GOListNode[TimedVal[Dot]]] = deriveDecoder
  implicit val GOListNodeEncoder: Encoder[GOListNode[TimedVal[Dot]]] = deriveEncoder

  implicit val DotTimedValDecoder: Decoder[TimedVal[Dot]] = deriveDecoder
  implicit val DotTimedValEncoder: Encoder[TimedVal[Dot]] = deriveEncoder

  implicit val ElemDecoder: Decoder[Elem[TimedVal[Dot]]] = deriveDecoder
  implicit val ElemEncoder: Encoder[Elem[TimedVal[Dot]]] = deriveEncoder

  implicit val GOListDecoder: Decoder[GOList.State[Dot]] =
    Decoder.decodeList[(GOListNode[TimedVal[Dot]], Elem[TimedVal[Dot]])].map(_.toMap)
  implicit val GOListEncoder: Encoder[GOList.State[Dot]] =
    Encoder.encodeList[(GOListNode[TimedVal[Dot]], Elem[TimedVal[Dot]])].contramap(_.toList)

  implicit val TodoTaskTimedValDecoder: Decoder[TimedVal[Posting]] = deriveDecoder
  implicit val TodoTaskTimedValEncoder: Encoder[TimedVal[Posting]] = deriveEncoder

  implicit val RGANodeDecoder: Decoder[RGANode[Posting]] = deriveDecoder
  implicit val RGANodeEncoder: Encoder[RGANode[Posting]] = deriveEncoder

  implicit val DotFunDecoder: Decoder[DotFun[RGANode[Posting]]] =
    Decoder.decodeList[(Dot, RGANode[Posting])].map(_.toMap)
  implicit val DotFunEncoder: Encoder[DotFun[RGANode[Posting]]] =
    Encoder.encodeList[(Dot, RGANode[Posting])].contramap(_.toList)

  implicit val CausalDecoder: Decoder[Causal[DotFun[RGANode[Posting]], DietMapCContext]] =
    deriveDecoder
  implicit val CausalEncoder: Encoder[Causal[DotFun[RGANode[Posting]], DietMapCContext]] =
    deriveEncoder

  implicit val rrgaEncoder: Encoder[RRGA.State[Posting, DietMapCContext]] =
    deriveEncoder[RRGA.State[Posting, DietMapCContext]]
  implicit val rrgaDecoder: Decoder[RRGA.State[Posting, DietMapCContext]] =
    deriveDecoder[RRGA.State[Posting, DietMapCContext]]

  implicit val _Tbm: IdenticallyTransmittable[RRGA.State[Posting, DietMapCContext]] = IdenticallyTransmittable()

  val crdtDescriptions: Binding[RRGA.State[Posting, DietMapCContext] => Unit] {
    type RemoteCall = RRGA.State[Posting, DietMapCContext] => Future[Unit]
  } = Binding("postings"): @scala.annotation.nowarn
}
