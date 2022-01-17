
package benchmarks.encrdt

import kofre.encrdt.causality.DotStore.Dot
import kofre.encrdt.crdts.{AddWinsLastWriterWinsMap, DeltaAddWinsLastWriterWinsMap}

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

import java.time.Instant

object Codecs {
  implicit val awlwwmapJsonCodec: JsonValueCodec[AddWinsLastWriterWinsMap.LatticeType[String, String]] =
    JsonCodecMaker.make[AddWinsLastWriterWinsMap.LatticeType[String, String]]

  implicit val dotMapAsSetCodec: JsonValueCodec[Set[(Dot, (String, (Instant, String)))]] = JsonCodecMaker.make
  implicit val dotMapCodec: JsonValueCodec[Map[Dot, (String, (Instant, String))]] = new JsonValueCodec[Map[Dot, (String, (Instant, String))]] {
    override def decodeValue(in: JsonReader, default: Map[Dot, (String, (Instant, String))]): Map[Dot, (String, (Instant, String))] =
      dotMapAsSetCodec.decodeValue(in, Set.empty).toMap

    override def encodeValue(x: Map[Dot, (String, (Instant, String))], out: JsonWriter): Unit =
      dotMapAsSetCodec.encodeValue(x.toSet, out)

    override def nullValue: Map[Dot, (String, (Instant, String))] =
      Map.empty
  }

  implicit val deltaAwlwwmapJsonCodec: JsonValueCodec[DeltaAddWinsLastWriterWinsMap.StateType[String, String]] = JsonCodecMaker.make
}
