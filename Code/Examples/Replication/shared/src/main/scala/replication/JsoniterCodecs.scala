package replication

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import kofre.base.Id
import kofre.datatypes.RGA.RGANode
import kofre.datatypes.{
  AddWinsSet, EnableWinsFlag, Epoche, GrowOnlyMap, GrowOnlyCounter, ObserveRemoveMap, PosNegCounter, RGA, TimedVal,
  TwoPhaseSet, GrowOnlySet
}
import kofre.decompose.interfaces.GrowOnlyList
import kofre.decompose.interfaces.GrowOnlyList.{GListElem, GListNode}
import kofre.decompose.interfaces.MVRegisterInterface.MVRegister
import kofre.decompose.interfaces.ResettableCounter
import kofre.dotted.Dotted
import kofre.protocol.AuctionInterface.AuctionData
import kofre.time.{ArrayRanges, Dot, Dots}
import kofre.base.Time

import scala.annotation.nowarn

object JsoniterCodecs {

  def bimapCodec[A, B](codec: JsonValueCodec[A], to: A => B, from: B => A): JsonValueCodec[B] = new JsonValueCodec[B]:
    override def decodeValue(in: JsonReader, default: B): B = to(codec.decodeValue(in, from(default)))
    override def encodeValue(x: B, out: JsonWriter): Unit   = codec.encodeValue(from(x), out)
    override def nullValue: B                               = to(codec.nullValue)

  /** Causal Context */
  implicit val arrayOfLongCodec: JsonValueCodec[Array[Time]] = JsonCodecMaker.make

  implicit val arrayRangesCodec: JsonValueCodec[ArrayRanges] = bimapCodec(
    arrayOfLongCodec,
    ar => new ArrayRanges(ar, ar.length),
    x => x.inner.slice(0, x.used)
  )

  implicit val idKeyCodec: JsonKeyCodec[kofre.base.Id] = new JsonKeyCodec[Id]:
    override def decodeKey(in: JsonReader): Id           = Id.predefined(in.readKeyAsString())
    override def encodeKey(x: Id, out: JsonWriter): Unit = out.writeKey(Id.unwrap(x))
  implicit val CausalContextCodec: JsonValueCodec[Dots] = JsonCodecMaker.make

  /** AddWinsSet */

  implicit def AWSetStateCodec[E: JsonValueCodec]: JsonValueCodec[AddWinsSet[E]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  given JsonValueCodec[Id] = bimapCodec(
    JsonCodecMaker.make[String],
    Id.predefined,
    Id.unwrap
  )
  implicit def AWSetEmbeddedCodec[E: JsonValueCodec]: JsonValueCodec[Map[E, Set[Dot]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** EWFlag */

  // implicit def EWFlagStateCodec: JsonValueCodec[Dotted[Dots]] = JsonCodecMaker.make
  implicit def EWFlagStateCodec: JsonValueCodec[EnableWinsFlag] = JsonCodecMaker.make

  // implicit def EWFlagEmbeddedCodec: JsonValueCodec[Set[Dot]] = JsonCodecMaker.make

  /** GCounter */
  implicit def MapStringIntStateCodec: JsonValueCodec[Map[String, Int]] = JsonCodecMaker.make
  implicit def GCounterStateCodec: JsonValueCodec[GrowOnlyCounter]      = JsonCodecMaker.make

  /** GrowOnlyList */
  @nowarn()
  implicit def growOnlyListCodec[E: JsonValueCodec]: JsonValueCodec[GrowOnlyList[E]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** GrowOnlySet */
  implicit def GSetStateCodec[E: JsonValueCodec]: JsonValueCodec[GrowOnlySet[E]] = JsonCodecMaker.make

  /** LastWriterWins */
  implicit def LastWriterWinsStateCodec[A: JsonValueCodec]: JsonValueCodec[Dotted[Map[Dot, TimedVal[A]]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  implicit def LastWriterWinsEmbeddedCodec[A: JsonValueCodec]: JsonValueCodec[Map[Dot, TimedVal[A]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))


  /** MVRegister */

  implicit def MVRegisterEmbeddedCodec[A: JsonValueCodec]: JsonValueCodec[MVRegister[A]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** ObserveRemoveMap */
  implicit def ORMapStateCodec[K: JsonValueCodec, V: JsonValueCodec]: JsonValueCodec[ObserveRemoveMap[K, V]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** PNCounter */

  implicit def PNCounterStateCodec: JsonValueCodec[PosNegCounter] = JsonCodecMaker.make

  /** ResettableCounter */

  implicit def RCounterStateCodec: JsonValueCodec[ResettableCounter] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** RGA */

  @nowarn()
  implicit def RGAStateCodec[E: JsonValueCodec]: JsonValueCodec[Dotted[RGA[E]]] = {
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
  }

  /** Rubis */

  implicit def RubisStateCodec: JsonValueCodec[(
      Dotted[Map[(String, String), Set[Dot]]],
      Map[String, String],
      Map[String, AuctionData]
  )] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** TwoPhaseSet */

  implicit def TwoPSetStateCodec[E: JsonValueCodec]: JsonValueCodec[TwoPhaseSet[E]] = JsonCodecMaker.make

  implicit def withContextWrapper[E: JsonValueCodec]: JsonValueCodec[Dotted[E]] = JsonCodecMaker.make

  implicit def twoPSetContext[E: JsonValueCodec]: JsonValueCodec[Dotted[TwoPhaseSet[E]]] =
    withContextWrapper(TwoPSetStateCodec)

  implicit def spcecificCodec: JsonValueCodec[Dotted[GrowOnlyMap[Int, AddWinsSet[Int]]]] =
    JsonCodecMaker.make[Dotted[Map[Int, AddWinsSet[Int]]]].asInstanceOf

}
