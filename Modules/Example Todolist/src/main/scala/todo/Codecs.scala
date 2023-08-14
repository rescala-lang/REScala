package todo

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import kofre.base.Uid
import kofre.datatypes.LastWriterWins
import kofre.datatypes.contextual.ReplicatedList
import kofre.dotted.{DotFun, Dotted}
import kofre.syntax.DeltaBuffer
import kofre.time.Dot
import loci.transmitter.IdenticallyTransmittable
import rescala.extra.replication.DeltaFor


object Codecs {

  implicit val taskRefCodec: JsonValueCodec[TaskRef] = JsonCodecMaker.make
  implicit val dotKeyCodec: JsonKeyCodec[Dot] = new JsonKeyCodec[Dot] {
    override def decodeKey(in: JsonReader): Dot = {
      val Array(time, id) = in.readKeyAsString().split("-", 2)
      Dot(Uid.predefined(id), time.toLong)
    }
    override def encodeKey(x: Dot, out: JsonWriter): Unit = out.writeKey(s"${x.time}-${x.place}")
  }
  implicit val idCodec: JsonValueCodec[Uid] = JsonCodecMaker.make[String].asInstanceOf
  implicit val idKeyCodec: JsonKeyCodec[kofre.base.Uid] = new JsonKeyCodec[Uid]:
    override def decodeKey(in: JsonReader): Uid           = Uid.predefined(in.readKeyAsString())
    override def encodeKey(x: Uid, out: JsonWriter): Unit = out.writeKey(Uid.unwrap(x))

  implicit val codecState: JsonValueCodec[Dotted[ReplicatedList[TaskRef]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
  implicit val codecRGA: JsonValueCodec[DeltaBuffer[Dotted[ReplicatedList[TaskRef]]]] =
    new JsonValueCodec[DeltaBuffer[Dotted[ReplicatedList[TaskRef]]]] {
      override def decodeValue(
          in: JsonReader,
          default: DeltaBuffer[Dotted[ReplicatedList[TaskRef]]]
      ): DeltaBuffer[Dotted[ReplicatedList[TaskRef]]] = {
        val state = codecState.decodeValue(in, default.state)
        DeltaBuffer[Dotted[ReplicatedList[TaskRef]]](state)
      }
      override def encodeValue(x: DeltaBuffer[Dotted[ReplicatedList[TaskRef]]], out: JsonWriter): Unit =
        codecState.encodeValue(x.state, out)
      override def nullValue: DeltaBuffer[Dotted[ReplicatedList[TaskRef]]] =
        DeltaBuffer(Dotted(ReplicatedList.empty[TaskRef]))
    }

  implicit val transmittableList: IdenticallyTransmittable[DeltaFor[ReplicatedList[TaskRef]]] =
    IdenticallyTransmittable()
  implicit val codectDeltaForTasklist: JsonValueCodec[DeltaFor[ReplicatedList[TaskRef]]] = JsonCodecMaker.make

  implicit val codecDeltaForLWW: JsonValueCodec[DeltaFor[LastWriterWins[Option[TaskData]]]] = JsonCodecMaker.make

  implicit val transmittableDeltaForLWW: IdenticallyTransmittable[DeltaFor[LastWriterWins[Option[TaskData]]]] =
    IdenticallyTransmittable()

  implicit val codecLww: JsonValueCodec[DeltaBuffer[Dotted[LastWriterWins[Option[TaskData]]]]] = JsonCodecMaker.make

}
