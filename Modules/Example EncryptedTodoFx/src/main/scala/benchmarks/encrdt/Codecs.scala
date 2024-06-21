package benchmarks.encrdt

import benchmarks.encrdt.todolist.ToDoEntry
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import encrdtlib.container.{AddWinsLastWriterWinsMap, DeltaAddWinsLastWriterWinsMap}
import rdts.base.Uid
import rdts.base.Uid.asId
import rdts.time.{ArrayRanges, Dot, Dots, Time}
import replication.JsoniterCodecs.given

import java.util.UUID

object Codecs {

  given dotsCodec: JsonValueCodec[Dots] = JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

  implicit val awlwwmapJsonCodec: JsonValueCodec[AddWinsLastWriterWinsMap.LatticeType[String, String]] =
    JsonCodecMaker.make(CodecMakerConfig.withSetMaxInsertNumber(Int.MaxValue).withMapMaxInsertNumber(Int.MaxValue))

  implicit val deltaAwlwwmapJsonCodec: JsonValueCodec[DeltaAddWinsLastWriterWinsMap.StateType[String, String]] =
    JsonCodecMaker.make(CodecMakerConfig.withSetMaxInsertNumber(Int.MaxValue).withMapMaxInsertNumber(Int.MaxValue))

  implicit val lamportClockKeyCodec: JsonKeyCodec[Dot] = new JsonKeyCodec[Dot] {
    override def decodeKey(in: JsonReader): Dot = {
      val inputString = in.readKeyAsString()
      val index       = inputString.indexOf('@')
      Dot(inputString.substring(index + 1).asId, inputString.substring(0, index).toLong)
    }

    override def encodeKey(x: Dot, out: JsonWriter): Unit = out.writeKey(s"${x.time}@${x.place}")
  }

  implicit val toDoMapCodec: JsonValueCodec[DeltaAddWinsLastWriterWinsMap.StateType[UUID, ToDoEntry]] =
    JsonCodecMaker.make(CodecMakerConfig.withSetMaxInsertNumber(Int.MaxValue).withMapMaxInsertNumber(Int.MaxValue))
}
