package todo

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rdts.base.Uid
import rdts.datatypes.LastWriterWins
import rdts.datatypes.contextual.ReplicatedList
import rdts.dotted.Dotted
import rdts.syntax.DeltaBuffer
import rdts.time.Dot

object Codecs {

  given taskRefCodec: JsonValueCodec[TaskRef] = JsonCodecMaker.make
  given idCodec: JsonValueCodec[Uid]          = JsonCodecMaker.make[String].asInstanceOf
  given codecState: JsonValueCodec[Dotted[ReplicatedList[TaskRef]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
  given codecRGA: JsonValueCodec[DeltaBuffer[Dotted[ReplicatedList[TaskRef]]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
  given codectDeltaForTasklist: JsonValueCodec[ReplicatedList[TaskRef]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
  given codecDeltaForLWW: JsonValueCodec[LastWriterWins[Option[TaskData]]] = JsonCodecMaker.make
  given codecDottedLWWOpt: JsonValueCodec[Dotted[LastWriterWins[Option[TaskData]]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
  given codecDottedLWW: JsonValueCodec[Dotted[LastWriterWins[TaskData]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
  given codecLww: JsonValueCodec[DeltaBuffer[Dotted[LastWriterWins[Option[TaskData]]]]] = JsonCodecMaker.make

}
