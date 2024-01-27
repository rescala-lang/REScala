package copl

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import loci.transmitter.transmittable.IdenticallyTransmittable
import rescala.extra.lattices.sequences.RGA.RGA
import rescala.extra.lattices.sequences.{LatticeSequence, RGA, Vertex}
import rescala.extra.lattices.sets.TwoPSet

object Codecs {

  implicit val stringCodec: JsonValueCodec[String]         = JsonCodecMaker.make[String]
  implicit val listCodec  : JsonValueCodec[List[Chatline]] = JsonCodecMaker.make[List[Chatline]]

  implicit val rgaTransmittable = IdenticallyTransmittable[Epoche[RGA[Chatline]]]()

  type Rgatuple = (TwoPSet[Vertex], Map[Vertex, Vertex], Map[Vertex, Chatline])
  def rgaToTuple(rga: RGA[Chatline]): Rgatuple = rga match {
    case LatticeSequence(vertices: TwoPSet[Vertex], edges: Map[Vertex, Vertex], values: Map[Vertex, Chatline]) =>
      (vertices, edges, values)
  }

  val vcodec: JsonValueCodec[(TwoPSet[Vertex], Map[Vertex, Vertex], Map[Vertex, Chatline])] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  implicit val rgaCodec: JsonValueCodec[RGA[Chatline]] = new JsonValueCodec[RGA[Chatline]] {
    override def decodeValue(in: JsonReader, default: RGA[Chatline]): RGA[Chatline] = {
      val (vertices, edges, values) = vcodec.decodeValue(in, rgaToTuple(default))
      LatticeSequence(vertices, edges, values)
    }
    override def encodeValue(x: RGA[Chatline], out: JsonWriter): Unit =
      vcodec.encodeValue(rgaToTuple(x), out)

    override def nullValue: RGA[Chatline] = RGA.empty[Chatline]
  }

  implicit val epocheCodec  : JsonValueCodec[Epoche[RGA[Chatline]]] = JsonCodecMaker.make[Epoche[RGA[Chatline]]]


}
