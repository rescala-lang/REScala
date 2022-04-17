package todo

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import kofre.decompose.interfaces.LWWRegisterInterface.LWWRegister
import kofre.decompose.interfaces.RGAInterface.RGA
import loci.transmitter.IdenticallyTransmittable
import rescala.extra.lattices.delta.JsoniterCodecs._
import rescala.extra.lattices.delta.crdt.reactive.ReactiveDeltaCRDT
import todo.Todolist.replicaId

object Codecs {

  implicit val taskRefCodec: JsonValueCodec[TaskRef] = JsonCodecMaker.make

  implicit val codecState: JsonValueCodec[RGA[TaskRef]] = RGAStateCodec
  implicit val codecRGA: JsonValueCodec[ReactiveDeltaCRDT[RGA[TaskRef]]] =
    new JsonValueCodec[ReactiveDeltaCRDT[RGA[TaskRef]]] {
      override def decodeValue(
          in: JsonReader,
          default: ReactiveDeltaCRDT[RGA[TaskRef]]
      ): ReactiveDeltaCRDT[RGA[TaskRef]] = {
        val state = codecState.decodeValue(in, default.state)
        new ReactiveDeltaCRDT[RGA[TaskRef]](state, replicaId, List())
      }
      override def encodeValue(x: ReactiveDeltaCRDT[RGA[TaskRef]], out: JsonWriter): Unit =
        codecState.encodeValue(x.state, out)
      override def nullValue: ReactiveDeltaCRDT[RGA[TaskRef]] = ReactiveDeltaCRDT[RGA[TaskRef]](replicaId)
    }

  implicit val transmittableList: IdenticallyTransmittable[RGA[TaskRef]] =
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
