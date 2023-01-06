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
import kofre.decompose.interfaces.LexCounterInterface.{LexCounter, LexPair}
import kofre.decompose.interfaces.MVRegisterInterface.MVRegister
import kofre.decompose.interfaces.RCounterInterface.RCounter
import kofre.dotted.Dotted
import kofre.protocol.AuctionInterface.AuctionData
import kofre.time.{ArrayRanges, Dot, Dots}
import kofre.base.Time

import scala.annotation.nowarn

object JsoniterCodecs {

  /** Causal Context */

  implicit val arrayOfLongCodec: JsonValueCodec[Array[Time]] = JsonCodecMaker.make

  implicit val arrayRangesCodec: JsonValueCodec[ArrayRanges] =
    new JsonValueCodec[ArrayRanges] {
      override def decodeValue(in: JsonReader, default: ArrayRanges): ArrayRanges = {
        val ar = arrayOfLongCodec.decodeValue(in, Array())
        new ArrayRanges(ar, ar.length)
      }
      override def encodeValue(x: ArrayRanges, out: JsonWriter): Unit =
        arrayOfLongCodec.encodeValue(x.inner.slice(0, x.used), out)
      override def nullValue: ArrayRanges = null
    }

  implicit val idKeyCodec: JsonKeyCodec[kofre.base.Id] = new JsonKeyCodec[Id]:
    override def decodeKey(in: JsonReader): Id           = Id.predefined(in.readKeyAsString())
    override def encodeKey(x: Id, out: JsonWriter): Unit = out.writeKey(Id.unwrap(x))
  implicit val CausalContextCodec: JsonValueCodec[Dots] = JsonCodecMaker.make

  /** AddWinsSet */

  @nowarn("msg=never used")
  implicit def AWSetStateCodec[E: JsonValueCodec]: JsonValueCodec[AddWinsSet[E]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  given JsonValueCodec[Id] = JsonCodecMaker.make[String].asInstanceOf[JsonValueCodec[Id]]
  @nowarn("msg=never used")
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

  @nowarn("msg=never used")
  implicit def GSetStateCodec[E: JsonValueCodec]: JsonValueCodec[GrowOnlySet[E]] =
    JsonCodecMaker.make[Set[E]].asInstanceOf

  /** LastWriterWins */
  @nowarn("msg=never used")
  implicit def LastWriterWinsStateCodec[A: JsonValueCodec]: JsonValueCodec[Dotted[Map[Dot, TimedVal[A]]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  @nowarn("msg=never used")
  implicit def LastWriterWinsEmbeddedCodec[A: JsonValueCodec]: JsonValueCodec[Map[Dot, TimedVal[A]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** LexCounter */

  implicit def LexCounterStateCodec: JsonValueCodec[LexCounter] = JsonCodecMaker.make

  /** MVRegister */

  @nowarn("msg=never used")
  implicit def MVRegisterEmbeddedCodec[A: JsonValueCodec]: JsonValueCodec[MVRegister[A]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** ObserveRemoveMap */
  @nowarn("msg=never used")
  implicit def ORMapStateCodec[K: JsonValueCodec, V: JsonValueCodec]: JsonValueCodec[ObserveRemoveMap[K, V]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** PNCounter */

  implicit def PNCounterStateCodec: JsonValueCodec[PosNegCounter] = JsonCodecMaker.make

  /** RCounter */

  implicit def RCounterStateCodec: JsonValueCodec[RCounter] =
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

  @nowarn("msg=never used")
  implicit def TwoPSetStateCodec[E: JsonValueCodec]: JsonValueCodec[TwoPhaseSet[E]] = JsonCodecMaker.make

  @nowarn("msg=never used")
  implicit def withContextWrapper[E: JsonValueCodec]: JsonValueCodec[Dotted[E]] = {
    def dottedTuple: JsonValueCodec[(E, Dots)] = JsonCodecMaker.make
    new JsonValueCodec[Dotted[E]] {
      override def decodeValue(in: JsonReader, default: Dotted[E]): Dotted[E] = {
        val (a, b) = dottedTuple.decodeValue(in, if (default == null) null else (default.store, default.context))
        Dotted(a, b)
      }
      override def encodeValue(x: Dotted[E], out: JsonWriter): Unit = dottedTuple.encodeValue((x.store, x.context), out)
      override def nullValue: Dotted[E]                             = null
    }
  }

  implicit def twoPSetContext[E: JsonValueCodec]: JsonValueCodec[Dotted[TwoPhaseSet[E]]] =
    withContextWrapper(TwoPSetStateCodec)

  implicit def spcecificCodec: JsonValueCodec[Dotted[GrowOnlyMap[Int, AddWinsSet[Int]]]] =
    JsonCodecMaker.make[Dotted[Map[Int, AddWinsSet[Int]]]].asInstanceOf

}
