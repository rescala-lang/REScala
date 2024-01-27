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

  implicit val stringCodec: JsonValueCodec[String]       = JsonCodecMaker.make[String]
  implicit val listCodec: JsonValueCodec[List[Chatline]] = JsonCodecMaker.make[List[Chatline]]

  implicit val rgaTransmittable: IdenticallyTransmittable[Epoche[Dotted[ReplicatedList[Chatline]]]] =
    IdenticallyTransmittable[Epoche[Dotted[ReplicatedList[Chatline]]]]()

  implicit val uidCodec: JsonValueCodec[Uid] = JsonCodecMaker.make[String].asInstanceOf[JsonValueCodec[Uid]]

  implicit val epocheCodec: JsonValueCodec[Epoche[Dotted[ReplicatedList[Chatline]]]] =
    JsonCodecMaker.make[Epoche[Dotted[ReplicatedList[Chatline]]]](CodecMakerConfig.withMapAsArray(true))

}
