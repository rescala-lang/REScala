package todo

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rdts.base.Uid
import rdts.datatypes.LastWriterWins
import rdts.datatypes.contextual.ReplicatedList
import rdts.dotted.Dotted
import rdts.syntax.DeltaBuffer
import rdts.time.Dot
import replication.JsoniterCodecs.given

object Codecs {

  given codecRGA: JsonValueCodec[DeltaBuffer[Dotted[ReplicatedList[TaskRef]]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
  given codecLww: JsonValueCodec[DeltaBuffer[LastWriterWins[Option[TaskData]]]] = JsonCodecMaker.make

}
