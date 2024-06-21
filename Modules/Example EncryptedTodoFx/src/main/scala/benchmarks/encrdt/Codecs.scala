package benchmarks.encrdt

import benchmarks.encrdt.todolist.ToDoEntry
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import encrdtlib.container.{AddWinsLastWriterWinsMap, DeltaAddWinsLastWriterWinsMap}
import rdts.base.Uid
import rdts.time.{ArrayRanges, Dot, Dots}
import replication.JsoniterCodecs.given

import java.util.UUID

object Codecs {

  given dotsCodec: JsonValueCodec[Dots] = JsonCodecMaker.make

  implicit val awlwwmapJsonCodec: JsonValueCodec[AddWinsLastWriterWinsMap.LatticeType[String, String]] =
    JsonCodecMaker.make(CodecMakerConfig.withSetMaxInsertNumber(Int.MaxValue).withMapMaxInsertNumber(Int.MaxValue))

  implicit val deltaAwlwwmapJsonCodec: JsonValueCodec[DeltaAddWinsLastWriterWinsMap.StateType[String, String]] =
    JsonCodecMaker.make(CodecMakerConfig.withSetMaxInsertNumber(Int.MaxValue).withMapMaxInsertNumber(Int.MaxValue).withMapAsArray(true))

  implicit val toDoMapCodec: JsonValueCodec[DeltaAddWinsLastWriterWinsMap.StateType[UUID, ToDoEntry]] =
    JsonCodecMaker.make(CodecMakerConfig.withSetMaxInsertNumber(Int.MaxValue).withMapMaxInsertNumber(Int.MaxValue).withMapAsArray(true))
}
