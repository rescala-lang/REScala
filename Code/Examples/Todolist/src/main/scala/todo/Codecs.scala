package todo

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import kofre.decompose.interfaces.LWWRegisterInterface.LWWRegister
import loci.transmitter.IdenticallyTransmittable
import rescala.extra.lattices.delta.JsoniterCodecs._
import rescala.extra.lattices.delta.crdt.reactive.{ListRDT, ReactiveDeltaCRDT}
import todo.Todolist.replicaId

object Codecs {

  implicit val taskRefCodec: JsonValueCodec[TaskRef] = JsonCodecMaker.make

  implicit val codecState: JsonValueCodec[ListRDT.State[TaskRef]] = RGAStateCodec
  implicit val codecRGA: JsonValueCodec[ListRDT[TaskRef]] =
    new JsonValueCodec[ListRDT[TaskRef]] {
      override def decodeValue(
          in: JsonReader,
          default: ListRDT[TaskRef]
      ): ListRDT[TaskRef] = {
        val state = codecState.decodeValue(in, default.state)
        new ListRDT[TaskRef](state, replicaId, List())
      }
      override def encodeValue(x: ListRDT[TaskRef], out: JsonWriter): Unit =
        codecState.encodeValue(x.state, out)
      override def nullValue: ListRDT[TaskRef] = ListRDT.empty[TaskRef](replicaId)
    }

  implicit val transmittableList: IdenticallyTransmittable[ListRDT.State[TaskRef]] =
    IdenticallyTransmittable()

  implicit val todoTaskCodec: JsonValueCodec[TaskData] = JsonCodecMaker.make

  implicit val codecLwwState: JsonValueCodec[LWWRegister[TaskData]] = JsonCodecMaker.make

  implicit val transmittableLWW: IdenticallyTransmittable[LWWRegister[TaskData]] =
    IdenticallyTransmittable()

  type LwC = ReactiveDeltaCRDT[LWWRegister[TaskData]]
  implicit val codecLww: JsonValueCodec[LwC] =
    new JsonValueCodec[LwC] {
      override def decodeValue(in: JsonReader, default: LwC): LwC = {
        val state: LWWRegister[TaskData] = codecLwwState.decodeValue(in, default.state)
        new ReactiveDeltaCRDT[LWWRegister[TaskData]](state, replicaId, List())
      }
      override def encodeValue(x: LwC, out: JsonWriter): Unit = codecLwwState.encodeValue(x.state, out)
      override def nullValue: LwC = {
        println(s"reading null")
        ReactiveDeltaCRDT[LWWRegister[TaskData]](replicaId)
      }
    }

}
