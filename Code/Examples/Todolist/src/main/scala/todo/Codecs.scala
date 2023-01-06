package todo

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import kofre.base.Id
import kofre.base.Id.asId
import kofre.datatypes.{ReplicatedList, TimedVal}
import kofre.time.Dot
import kofre.decompose.interfaces.CausalLastWriterWinsRegister
import kofre.deprecated.containers.DeltaBufferRDT
import kofre.dotted.{DotFun, Dotted}
import loci.transmitter.IdenticallyTransmittable
import rescala.extra.replication.DeltaFor
import todo.Todolist.replicaId

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
    override def decodeKey(in: JsonReader): Id = Id.predefined(in.readKeyAsString())
    override def encodeKey(x: Id, out: JsonWriter): Unit = out.writeKey(Id.unwrap(x))

  @nowarn()
  implicit val codecState: JsonValueCodec[Dotted[ReplicatedList[TaskRef]]] =
    JsonCodecMaker.make(CodecMakerConfig.withMapAsArray(true))
  implicit val codecRGA: JsonValueCodec[DeltaBufferRDT[ReplicatedList[TaskRef]]] =
    new JsonValueCodec[DeltaBufferRDT[ReplicatedList[TaskRef]]] {
      override def decodeValue(
          in: JsonReader,
          default: DeltaBufferRDT[ReplicatedList[TaskRef]]
      ): DeltaBufferRDT[ReplicatedList[TaskRef]] = {
        val state = codecState.decodeValue(in, default.state)
        new DeltaBufferRDT[ReplicatedList[TaskRef]](state, replicaId, List())
      }
      override def encodeValue(x: DeltaBufferRDT[ReplicatedList[TaskRef]], out: JsonWriter): Unit =
        codecState.encodeValue(x.state, out)
      override def nullValue: DeltaBufferRDT[ReplicatedList[TaskRef]] = DeltaBufferRDT(replicaId, ReplicatedList.empty[TaskRef])
    }

  implicit val transmittableList: IdenticallyTransmittable[DeltaFor[ReplicatedList[TaskRef]]] = IdenticallyTransmittable()
  implicit val codectDeltaForTasklist: JsonValueCodec[DeltaFor[ReplicatedList[TaskRef]]]      = JsonCodecMaker.make

  implicit val codecLwwState: JsonValueCodec[Dotted[DotFun[TimedVal[TaskData]]]] = JsonCodecMaker.make

  implicit val codecDeltaForLWW: JsonValueCodec[DeltaFor[CausalLastWriterWinsRegister[TaskData]]] = JsonCodecMaker.make

  implicit val transmittableDeltaForLWW: IdenticallyTransmittable[DeltaFor[CausalLastWriterWinsRegister[TaskData]]] =
    IdenticallyTransmittable()

  type LwC = DeltaBufferRDT[CausalLastWriterWinsRegister[TaskData]]
  implicit val codecLww: JsonValueCodec[LwC] = JsonCodecMaker.make

}
