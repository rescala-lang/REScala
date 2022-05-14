package rescala.extra.lattices.delta

import cats.collections.Diet
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import kofre.base.Defs.Time
import kofre.protocol.AuctionInterface.AuctionData
import kofre.decompose.interfaces.GListInterface.{Elem, GListNode}
import kofre.decompose.interfaces.RGA.RGANode
import kofre.decompose.TimedVal
import kofre.causality.{ArrayRanges, CausalContext, Dot}
import kofre.decompose.interfaces.LexCounterInterface.LexPair
import kofre.contextual.WithContext
import kofre.decompose.interfaces.{EnableWinsFlag, GMap, RGA}
import kofre.predef.{AddWinsSet, Epoche, GrowOnlyCounter, PosNegCounter}

import scala.annotation.nowarn

object JsoniterCodecs {

  /** Causal Context */

  implicit val SetCContextCodec: JsonValueCodec[Set[Dot]] = JsonCodecMaker.make

  implicit val DietCodec: JsonValueCodec[Diet[Long]] = new JsonValueCodec[Diet[Long]] {
    override def decodeValue(in: JsonReader, default: Diet[Long]): Diet[Long] = {
      var result = Diet.empty[Long]

      in.isNextToken('"')

      while (!in.isNextToken('"')) {
        in.rollbackToken()

        in.nextToken() match {
          case 'R' =>
            while (!in.isNextToken('(')) {}
            val lower = in.readLong()
            in.isNextToken(',')
            val upper = in.readLong()
            result = result + cats.collections.Range(lower, upper)
            in.isNextToken(')')
          case _ =>
        }
      }

      result
    }

    override def encodeValue(x: Diet[Long], out: JsonWriter): Unit = {
      out.writeVal(x.toString)
    }

    override def nullValue: Diet[Long] = null
  }

  implicit val arrayOfLongCodec: JsonValueCodec[Array[Time]] = JsonCodecMaker.make

  implicit val arrayRangesCodec: JsonValueCodec[ArrayRanges] =
    new JsonValueCodec[ArrayRanges] {
      override def decodeValue(in: JsonReader, default: ArrayRanges): ArrayRanges = {
        val ar = arrayOfLongCodec.decodeValue(in, Array())
        new ArrayRanges(ar, ar.length)
      }
      override def encodeValue(x: ArrayRanges, out: JsonWriter): Unit = arrayOfLongCodec.encodeValue(x.inner.slice(0, x.used), out)
      override def nullValue: ArrayRanges = null
    }

  implicit val CausalContextCodec: JsonValueCodec[CausalContext] =
    JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))

  /** AddWinsSet */

  @nowarn("msg=never used")
  implicit def AWSetStateCodec[E: JsonValueCodec]: JsonValueCodec[AddWinsSet[E]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  @nowarn("msg=never used")
  implicit def AWSetEmbeddedCodec[E: JsonValueCodec]: JsonValueCodec[Map[E, Set[Dot]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** EWFlag */

  // implicit def EWFlagStateCodec: JsonValueCodec[WithContext[CausalContext]] = JsonCodecMaker.make
  implicit def EWFlagStateCodec: JsonValueCodec[EnableWinsFlag] = JsonCodecMaker.make

  // implicit def EWFlagEmbeddedCodec: JsonValueCodec[Set[Dot]] = JsonCodecMaker.make

  /** GCounter */
  implicit def MapStringIntStateCodec: JsonValueCodec[Map[String, Int]] = JsonCodecMaker.make
  implicit def GCounterStateCodec: JsonValueCodec[GrowOnlyCounter]      = JsonCodecMaker.make

  /** GList */

  @nowarn("msg=never used")
  implicit def GListStateCodec[E: JsonValueCodec]: JsonValueCodec[Map[GListNode[TimedVal[E]], Elem[TimedVal[E]]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** GSet */

  @nowarn("msg=never used")
  implicit def GSetStateCodec[E: JsonValueCodec]: JsonValueCodec[Set[E]] = JsonCodecMaker.make

  /** LastWriterWins */
  @nowarn("msg=never used")
  implicit def LastWriterWinsStateCodec[A: JsonValueCodec]: JsonValueCodec[WithContext[Map[Dot, TimedVal[A]]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  @nowarn("msg=never used")
  implicit def LastWriterWinsEmbeddedCodec[A: JsonValueCodec]: JsonValueCodec[Map[Dot, TimedVal[A]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** LexCounter */

  implicit def LexCounterStateCodec: JsonValueCodec[Map[String, LexPair[Int, Int]]] = JsonCodecMaker.make

  /** MVRegister */

  @nowarn("msg=never used")
  implicit def MVRegisterStateCodec[A: JsonValueCodec]: JsonValueCodec[WithContext[Map[Dot, A]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  @nowarn("msg=never used")
  implicit def MVRegisterEmbeddedCodec[A: JsonValueCodec]: JsonValueCodec[Map[Dot, A]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** ORMap */
  @nowarn("msg=never used")
  implicit def ORMapStateCodec[K: JsonValueCodec, V: JsonValueCodec]: JsonValueCodec[WithContext[Map[K, V]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  @nowarn("msg=never used")
  implicit def ORMapEmbeddedCodec[K: JsonValueCodec, V: JsonValueCodec]: JsonValueCodec[Map[K, V]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** PNCounter */

  implicit def PNCounterStateCodec: JsonValueCodec[PosNegCounter] = JsonCodecMaker.make

  /** RCounter */

  implicit def RCounterStateCodec: JsonValueCodec[WithContext[Map[Dot, (Int, Int)]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  implicit def RCounterEmbeddedCodec: JsonValueCodec[Map[Dot, (Int, Int)]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** RGA */

  @nowarn("msg=never used")
  implicit def RGAStateCodec[E: JsonValueCodec]: JsonValueCodec[WithContext[RGA[E]]] = {
    implicit def RGAleftCodec
        : JsonValueCodec[Epoche[Map[GListNode[TimedVal[Dot]], Elem[TimedVal[Dot]]]]] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

    implicit def RGArightCodec: JsonValueCodec[Map[Dot, RGANode[E]]] =
      JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
    JsonCodecMaker.make
  }

  /** Rubis */

  implicit def RubisStateCodec: JsonValueCodec[(
      WithContext[Map[(String, String), Set[Dot]]],
      Map[String, String],
      Map[String, AuctionData]
  )] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** TwoPSet */

  @nowarn("msg=never used")
  implicit def TwoPSetStateCodec[E: JsonValueCodec]: JsonValueCodec[(Set[E], Set[E])] = JsonCodecMaker.make

  @nowarn("msg=never used")
  implicit def gmapCodec[K: JsonKeyCodec, V: JsonValueCodec]: JsonValueCodec[GMap[K, V]] = JsonCodecMaker.make




  @nowarn("msg=never used")
  implicit def withContextWrapper[E: JsonValueCodec]: JsonValueCodec[WithContext[E]] = JsonCodecMaker.make


  implicit def spcecificCodec:  JsonValueCodec[WithContext[GMap[Int, AddWinsSet[Int]]]] = JsonCodecMaker.make

}
