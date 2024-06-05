package calendar

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rdts.base.Uid
import rdts.datatypes.LastWriterWins
import rdts.datatypes.contextual.{ReplicatedList, ReplicatedSet}
import rdts.dotted.Dotted
import rdts.syntax.DeltaBuffer
import rdts.time.Dot

object Codecs {

  given taskRefCodec: JsonValueCodec[Appointment] = JsonCodecMaker.make
  given codecState: JsonValueCodec[Dotted[ReplicatedSet[Appointment]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
  given codecRGA: JsonValueCodec[DeltaBuffer[Dotted[ReplicatedSet[Appointment]]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
  given codecTupledRGA: JsonValueCodec[Tuple1[DeltaBuffer[Dotted[ReplicatedSet[Appointment]]]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
  given codecDeltaForTasklist: JsonValueCodec[ReplicatedSet[Appointment]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
  given codecDeltaForLWW: JsonValueCodec[LastWriterWins[Option[Appointment]]] = JsonCodecMaker.make
  given codecDottedLWWOpt: JsonValueCodec[Dotted[LastWriterWins[Option[Appointment]]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
  given codecDottedLWW: JsonValueCodec[Dotted[LastWriterWins[Appointment]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
  given codecLww: JsonValueCodec[DeltaBuffer[Dotted[LastWriterWins[Option[Appointment]]]]] = JsonCodecMaker.make

}
