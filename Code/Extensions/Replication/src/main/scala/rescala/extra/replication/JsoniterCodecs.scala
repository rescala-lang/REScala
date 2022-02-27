package rescala.extra.lattices.delta

import cats.collections.Diet
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import kofre.decompose.interfaces.AuctionInterface.AuctionData
import kofre.decompose.interfaces.ForcedWriteInterface.State
import kofre.decompose.interfaces.GListInterface.{Elem, GListNode}
import kofre.decompose.interfaces.RGAInterface.RGANode
import kofre.decompose.{LexPair, TimedVal}
import kofre.causality.{CausalContext, Dot}
import kofre.dotbased.CausalStore

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

  implicit val intTreeCContextCodec: JsonValueCodec[CausalContext] =
    JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))

  /** AWSet */

  implicit def AWSetStateCodec[E: JsonValueCodec]: JsonValueCodec[CausalStore[Map[E, Set[Dot]]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  implicit def AWSetEmbeddedCodec[E: JsonValueCodec]: JsonValueCodec[Map[E, Set[Dot]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** EWFlag */

  implicit def EWFlagStateCodec: JsonValueCodec[CausalStore[Set[Dot]]] = JsonCodecMaker.make

  // implicit def EWFlagEmbeddedCodec: JsonValueCodec[Set[Dot]] = JsonCodecMaker.make

  /** GCounter */

  implicit def GCounterStateCodec: JsonValueCodec[Map[String, Int]] = JsonCodecMaker.make

  /** GList */

  implicit def GListStateCodec[E: JsonValueCodec]: JsonValueCodec[Map[GListNode[TimedVal[E]], Elem[TimedVal[E]]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** GSet */

  implicit def GSetStateCodec[E: JsonValueCodec]: JsonValueCodec[Set[E]] = JsonCodecMaker.make

  /** LastWriterWins */

  implicit def LastWriterWinsStateCodec[A: JsonValueCodec]
      : JsonValueCodec[CausalStore[Map[Dot, TimedVal[A]]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  implicit def LastWriterWinsEmbeddedCodec[A: JsonValueCodec]: JsonValueCodec[Map[Dot, TimedVal[A]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** LexCounter */

  implicit def LexCounterStateCodec: JsonValueCodec[Map[String, LexPair[Int, Int]]] = JsonCodecMaker.make

  /** MVRegister */

  implicit def MVRegisterStateCodec[A: JsonValueCodec]: JsonValueCodec[CausalStore[Map[Dot, A]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  implicit def MVRegisterEmbeddedCodec[A: JsonValueCodec]: JsonValueCodec[Map[Dot, A]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** ORMap */

  implicit def ORMapStateCodec[K: JsonValueCodec, V: JsonValueCodec]
      : JsonValueCodec[CausalStore[Map[K, V]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  implicit def ORMapEmbeddedCodec[K: JsonValueCodec, V: JsonValueCodec]: JsonValueCodec[Map[K, V]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** PNCounter */

  implicit def PNCounterStateCodec: JsonValueCodec[(Map[String, Int], Map[String, Int])] = JsonCodecMaker.make

  /** RCounter */

  implicit def RCounterStateCodec: JsonValueCodec[CausalStore[Map[Dot, (Int, Int)]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  implicit def RCounterEmbeddedCodec: JsonValueCodec[Map[Dot, (Int, Int)]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** RGA */

  implicit def RGAStateCodec[E: JsonValueCodec]
      : JsonValueCodec[CausalStore[(State[Map[GListNode[TimedVal[Dot]], Elem[TimedVal[Dot]]]], Map[Dot, RGANode[E]])]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** Rubis */

  implicit def RubisStateCodec: JsonValueCodec[(
      CausalStore[Map[(String, String), Set[Dot]]],
      Map[String, String],
      Map[String, AuctionData]
  )] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** TwoPSet */

  implicit def TwoPSetStateCodec[E: JsonValueCodec]: JsonValueCodec[(Set[E], Set[E])] = JsonCodecMaker.make
}
