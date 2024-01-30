package copl

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import kofre.base.Uid
import kofre.datatypes.TwoPhaseSet
import kofre.datatypes.contextual.ReplicatedList
import kofre.dotted.Dotted
import kofre.time.{Dot, Dots}
import loci.transmitter.IdenticallyTransmittable

object Codecs {

  given stringCodec: JsonValueCodec[String]       = JsonCodecMaker.make[String]
  given listCodec: JsonValueCodec[List[Chatline]] = JsonCodecMaker.make[List[Chatline]]

  given rgaTransmittable: IdenticallyTransmittable[Epoche[Dotted[ReplicatedList[Chatline]]]] =
    IdenticallyTransmittable[Epoche[Dotted[ReplicatedList[Chatline]]]]()

  // The cast here is because Uid actually is a string … but it is an opaque type so we should not be able to know that.
  // We do need to know what the actual datatype is for the codec though
  given uidCodec: JsonValueCodec[Uid] = stringCodec.asInstanceOf[JsonValueCodec[Uid]]


  given epocheCodec: JsonValueCodec[Epoche[Dotted[ReplicatedList[Chatline]]]] =
    JsonCodecMaker.make[Epoche[Dotted[ReplicatedList[Chatline]]]](CodecMakerConfig.withMapAsArray(true))

}
