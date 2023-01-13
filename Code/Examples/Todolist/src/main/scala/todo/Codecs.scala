package todo

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import kofre.base.Id
import kofre.base.Id.asId
import kofre.datatypes.{CausalLastWriterWins, ReplicatedList}
import kofre.time.Dot
import kofre.dotted.{DotFun, Dotted}
import loci.transmitter.IdenticallyTransmittable
import rescala.extra.replication.DeltaFor
import todo.Todolist.replicaId
import kofre.datatypes.LastWriterWins.TimedVal
import kofre.syntax.{DeltaBuffer, DeltaBufferDotted}

import scala.annotation.nowarn

object Codecs {

  implicit val taskRefCodec: JsonValueCodec[TaskRef] = JsonCodecMaker.make
  implicit val dotKeyCodec: JsonKeyCodec[Dot] = new JsonKeyCodec[Dot] {
    override def decodeKey(in: JsonReader): Dot = {
      val Array(time, id) = in.readKeyAsString().split("-", 2)
      Dot(Id.predefined(id), time.toLong)
    }
    override def encodeKey(x: Dot, out: JsonWriter): Unit = out.writeKey(s"${x.time}-${x.replicaId}")
  }
  implicit val idCodec: JsonValueCodec[Id] = JsonCodecMaker.make[String].asInstanceOf
  implicit val idKeyCodec: JsonKeyCodec[kofre.base.Id] = new JsonKeyCodec[Id]:
    override def decodeKey(in: JsonReader): Id           = Id.predefined(in.readKeyAsString())
    override def encodeKey(x: Id, out: JsonWriter): Unit = out.writeKey(Id.unwrap(x))

  @nowarn()
  implicit val codecState: JsonValueCodec[Dotted[ReplicatedList[TaskRef]]]          =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
  implicit val codecRGA: JsonValueCodec[DeltaBufferDotted[ReplicatedList[TaskRef]]] =
    new JsonValueCodec[DeltaBufferDotted[ReplicatedList[TaskRef]]] {
      override def decodeValue(
          in: JsonReader,
          default: DeltaBufferDotted[ReplicatedList[TaskRef]]
      ): DeltaBufferDotted[ReplicatedList[TaskRef]] = {
        val state = codecState.decodeValue(in, default.state)
        new DeltaBufferDotted[ReplicatedList[TaskRef]](replicaId, state, List())
      }
      override def encodeValue(x: DeltaBufferDotted[ReplicatedList[TaskRef]], out: JsonWriter): Unit =
        codecState.encodeValue(x.state, out)
      override def nullValue: DeltaBufferDotted[ReplicatedList[TaskRef]] =
        DeltaBuffer.dotted(replicaId, ReplicatedList.empty[TaskRef])
    }

  implicit val transmittableList: IdenticallyTransmittable[DeltaFor[ReplicatedList[TaskRef]]] =
    IdenticallyTransmittable()
  implicit val codectDeltaForTasklist: JsonValueCodec[DeltaFor[ReplicatedList[TaskRef]]] = JsonCodecMaker.make

  implicit val codecLwwState: JsonValueCodec[Dotted[DotFun[TimedVal[TaskData]]]] = JsonCodecMaker.make

  implicit val codecDeltaForLWW: JsonValueCodec[DeltaFor[CausalLastWriterWins[TaskData]]] = JsonCodecMaker.make

  implicit val transmittableDeltaForLWW: IdenticallyTransmittable[DeltaFor[CausalLastWriterWins[TaskData]]] =
    IdenticallyTransmittable()

  implicit val codecLww: JsonValueCodec[DeltaBufferDotted[CausalLastWriterWins[TaskData]]] = JsonCodecMaker.make

}
