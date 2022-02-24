package todo

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import loci.transmitter.IdenticallyTransmittable
import rescala.extra.lattices.delta.JsoniterCodecs._
import rescala.extra.lattices.delta.crdt.reactive
import rescala.extra.lattices.delta.crdt.reactive.{LWWRegister, ListRDT}
import todo.Todolist.replicaId

object Codecs {

  implicit val taskRefCodec: JsonValueCodec[TaskRef] = JsonCodecMaker.make

  implicit val codecState: JsonValueCodec[ListRDT.State[TaskRef]] = RGAStateCodec
  implicit val codecRGA: JsonValueCodec[ListRDT[TaskRef]]         =
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

  implicit val codecLwwState: JsonValueCodec[LWWRegister.State[TaskData]] = JsonCodecMaker.make

  implicit val transmittableLWW: IdenticallyTransmittable[LWWRegister.State[TaskData]] =
    IdenticallyTransmittable()

  type LwC = LWWRegister[TaskData]
  implicit val codecLww: JsonValueCodec[LwC] =
    new JsonValueCodec[LwC] {
      override def decodeValue(in: JsonReader, default: LwC): LwC = {
        val state: reactive.LWWRegister.State[TaskData] = codecLwwState.decodeValue(in, default.state)
        new LWWRegister[TaskData](state, replicaId, List())
      }
      override def encodeValue(x: LwC, out: JsonWriter): Unit = codecLwwState.encodeValue(x.state, out)
      override def nullValue: LwC = {
        println(s"reading null")
        LWWRegister[TaskData](replicaId)
      }
    }

}
