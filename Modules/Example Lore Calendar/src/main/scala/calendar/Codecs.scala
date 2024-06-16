package calendar

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import rdts.datatypes.contextual.ReplicatedSet
import rdts.dotted.Dotted
import rdts.syntax.DeltaBuffer

object Codecs {

  given codecRGA: JsonValueCodec[DeltaBuffer[Dotted[ReplicatedSet[Appointment]]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))

}
