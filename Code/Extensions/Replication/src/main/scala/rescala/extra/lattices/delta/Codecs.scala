package rescala.extra.lattices.delta

import cats.collections.Diet
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rescala.extra.lattices.delta.CContext.DietMapCContext
import rescala.extra.lattices.delta.crdt.{AuctionData, Elem, FW, GOListNode, RGANode}

object Codecs {

  /** Causal Context */

  implicit val SetCContextCodec: JsonValueCodec[Set[Dot]] = JsonCodecMaker.make

  implicit val DietCodec: JsonValueCodec[Diet[Int]] = new JsonValueCodec[Diet[Int]] {
    override def decodeValue(in: JsonReader, default: Diet[Int]): Diet[Int] = {
      var result = Diet.empty[Int]

      in.isNextToken('"')

      while (!in.isNextToken('"')) {
        in.rollbackToken()

        in.nextToken() match {
          case 'R' =>
            while (!in.isNextToken('(')) {}
            val lower = in.readInt()
            in.isNextToken(',')
            val upper = in.readInt()
            result = result + cats.collections.Range(lower, upper)
            in.isNextToken(')')
          case _ =>
        }
      }

      result
    }

    override def encodeValue(x: Diet[Int], out: JsonWriter): Unit = {
      out.writeVal(x.toString)
    }

    override def nullValue: Diet[Int] = null
  }

  implicit val DietMapCContextCodec: JsonValueCodec[DietMapCContext] = JsonCodecMaker.make

  /** AWSet */

  implicit def AWSetStateCodec[E: JsonValueCodec, C: JsonValueCodec]: JsonValueCodec[Causal[Map[E, Set[Dot]], C]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  implicit def AWSetEmbeddedCodec[E: JsonValueCodec]: JsonValueCodec[Map[E, Set[Dot]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** EWFlag */

  implicit def EWFlagStateCodec[C: JsonValueCodec]: JsonValueCodec[Causal[Set[Dot], C]] = JsonCodecMaker.make

  //implicit def EWFlagEmbeddedCodec: JsonValueCodec[Set[Dot]] = JsonCodecMaker.make

  /** GCounter */

  implicit def GCounterStateCodec: JsonValueCodec[Map[String, Int]] = JsonCodecMaker.make

  /** GOList */

  implicit def GOListStateCodec[E: JsonValueCodec]: JsonValueCodec[Map[GOListNode[TimedVal[E]], Elem[TimedVal[E]]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** GSet */

  implicit def GSetStateCodec[E: JsonValueCodec]: JsonValueCodec[Set[E]] = JsonCodecMaker.make

  /** LastWriterWins */

  implicit def LastWriterWinsStateCodec[A: JsonValueCodec, C: JsonValueCodec]
      : JsonValueCodec[Causal[Map[Dot, TimedVal[A]], C]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  implicit def LastWriterWinsEmbeddedCodec[A: JsonValueCodec]: JsonValueCodec[Map[Dot, TimedVal[A]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** LexCounter */

  implicit def LexCounterStateCodec: JsonValueCodec[Map[String, LexPair[Int, Int]]] = JsonCodecMaker.make

  /** MVRegister */

  implicit def MVRegisterStateCodec[A: JsonValueCodec, C: JsonValueCodec]: JsonValueCodec[Causal[Map[Dot, A], C]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  implicit def MVRegisterEmbeddedCodec[A: JsonValueCodec]: JsonValueCodec[Map[Dot, A]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** ORMap */

  implicit def ORMapStateCodec[K: JsonValueCodec, V: JsonValueCodec, C: JsonValueCodec]
      : JsonValueCodec[Causal[Map[K, V], C]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  implicit def ORMapEmbeddedCodec[K: JsonValueCodec, V: JsonValueCodec]: JsonValueCodec[Map[K, V]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** PNCounter */

  implicit def PNCounterStateCodec: JsonValueCodec[(Map[String, Int], Map[String, Int])] = JsonCodecMaker.make

  /** RCounter */

  implicit def RCounterStateCodec[C: JsonValueCodec]: JsonValueCodec[Causal[Map[Dot, (Int, Int)], C]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  implicit def RCounterEmbeddedCodec: JsonValueCodec[Map[Dot, (Int, Int)]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** RGA */

  implicit def RGAStateCodec[E: JsonValueCodec, C: JsonValueCodec]
      : JsonValueCodec[Causal[(FW[Map[GOListNode[TimedVal[Dot]], Elem[TimedVal[Dot]]]], Map[Dot, RGANode[E]]), C]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** Rubis */

  implicit def RubisStateCodec[C: JsonValueCodec]: JsonValueCodec[(
      Causal[Map[(String, String), Set[Dot]], C],
      Map[String, String],
      Map[String, AuctionData]
  )] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  /** TwoPSet */

  implicit def TwoPSetStateCodec[E: JsonValueCodec]: JsonValueCodec[(Set[E], Set[E])] = JsonCodecMaker.make
}
