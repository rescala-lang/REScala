package rescala.extra.lattices.delta

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import kofre.base.Defs.Time
import kofre.datatypes.RGA.RGANode
import kofre.datatypes.{
  AddWinsSet, EnableWinsFlag, Epoche, GrowMap, GrowOnlyCounter, ObserveRemoveMap, PosNegCounter, RGA, TimedVal,
  TwoPhaseSet
}
import kofre.decompose.interfaces.GListInterface.{GList, GListElem, GListNode}
import kofre.decompose.interfaces.LexCounterInterface.LexPair
import kofre.decompose.interfaces.MVRegisterInterface.MVRegister
import kofre.decompose.interfaces.RCounterInterface.RCounter
import kofre.dotted.Dotted
import kofre.protocol.AuctionInterface.AuctionData
import kofre.time.{ArrayRanges, Dot, Dots}

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

  implicit val CausalContextCodec: JsonValueCodec[Dots] = JsonCodecMaker.make

  /** AddWinsSet */

  @nowarn("msg=never used")
  implicit def AWSetStateCodec[E: JsonValueCodec]: JsonValueCodec[AddWinsSet[E]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

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

  /** GList */

  @nowarn("msg=never used")
  def mapArrayCodec[A: JsonValueCodec, B: JsonValueCodec]: JsonValueCodec[Map[A, B]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  @nowarn("msg=never used")
  def timedTupleCodec[A: JsonValueCodec]: JsonValueCodec[(A, String, Long, Long)] = JsonCodecMaker.make

  implicit def timedValCodec[A: JsonValueCodec]: JsonValueCodec[TimedVal[A]] = new JsonValueCodec[TimedVal[A]] {
    override def decodeValue(in: JsonReader, default: TimedVal[A]): TimedVal[A] = {
      val (a, b, c, d) = timedTupleCodec[A].decodeValue(
        in,
        if (default == null) null
        else (default.value, default.replicaID, default.nanoTime, default.timestamp)
      )
      TimedVal(a, b, c, d)
    }
    override def encodeValue(x: TimedVal[A], out: JsonWriter): Unit =
      timedTupleCodec[A].encodeValue((x.value, x.replicaID, x.nanoTime, x.timestamp), out)
    override def nullValue: TimedVal[A] = null
  }

  @nowarn()
  implicit def GListStateCodec[E: JsonValueCodec]: JsonValueCodec[GList[E]] = {
    mapArrayCodec[GListNode[TimedVal[E]], GListElem[TimedVal[E]]](JsonCodecMaker.make, JsonCodecMaker.make)
  }

  /** GSet */

  @nowarn("msg=never used")
  implicit def GSetStateCodec[E: JsonValueCodec]: JsonValueCodec[Set[E]] = JsonCodecMaker.make

  /** LastWriterWins */
  @nowarn("msg=never used")
  implicit def LastWriterWinsStateCodec[A: JsonValueCodec]: JsonValueCodec[Dotted[Map[Dot, TimedVal[A]]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  @nowarn("msg=never used")
  implicit def LastWriterWinsEmbeddedCodec[A: JsonValueCodec]: JsonValueCodec[Map[Dot, TimedVal[A]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** LexCounter */

  implicit def LexCounterStateCodec: JsonValueCodec[Map[String, LexPair[Int, Int]]] = JsonCodecMaker.make

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
    implicit def RGAleftCodec: JsonValueCodec[Epoche[Map[GListNode[TimedVal[Dot]], GListElem[TimedVal[Dot]]]]] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    implicit def RGArightCodec: JsonValueCodec[Map[Dot, RGANode[E]]] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
    JsonCodecMaker.make
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
  implicit def gmapCodec[K: JsonKeyCodec, V: JsonValueCodec]: JsonValueCodec[GrowMap[K, V]] = JsonCodecMaker.make

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

  implicit def spcecificCodec: JsonValueCodec[Dotted[GrowMap[Int, AddWinsSet[Int]]]] = JsonCodecMaker.make

}
