package todo

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import kofre.causality.CausalContext
import kofre.decompose.interfaces.LWWRegisterInterface.LWWRegister
import kofre.decompose.interfaces.RGAInterface.RGA
import loci.transmitter.IdenticallyTransmittable
import rescala.extra.lattices.delta.JsoniterCodecs._
import kofre.decompose.containers.DeltaBufferRDT
import todo.Todolist.replicaId

object Codecs {

  implicit val taskRefCodec: JsonValueCodec[TaskRef] = JsonCodecMaker.make

  implicit val codecState: JsonValueCodec[RGA[TaskRef]]               = RGAStateCodec
  implicit val codecRGA: JsonValueCodec[DeltaBufferRDT[RGA[TaskRef]]] =
    new JsonValueCodec[DeltaBufferRDT[RGA[TaskRef]]] {
      override def decodeValue(
          in: JsonReader,
          default: DeltaBufferRDT[RGA[TaskRef]]
      ): DeltaBufferRDT[RGA[TaskRef]] = {
        val state = codecState.decodeValue(in, default.state)
        new DeltaBufferRDT[RGA[TaskRef]](state, replicaId, CausalContext.empty, List())
      }
      override def encodeValue(x: DeltaBufferRDT[RGA[TaskRef]], out: JsonWriter): Unit =
        codecState.encodeValue(x.state, out)
      override def nullValue: DeltaBufferRDT[RGA[TaskRef]] = DeltaBufferRDT[RGA[TaskRef]](replicaId)
    }

  implicit val transmittableList: IdenticallyTransmittable[RGA[TaskRef]] =
    IdenticallyTransmittable()

  implicit val todoTaskCodec: JsonValueCodec[TaskData] = JsonCodecMaker.make

  implicit val codecLwwState: JsonValueCodec[LWWRegister[TaskData]] = JsonCodecMaker.make

  implicit val transmittableLWW: IdenticallyTransmittable[LWWRegister[TaskData]] =
    IdenticallyTransmittable()

  type LwC = DeltaBufferRDT[LWWRegister[TaskData]]
  implicit val codecLww: JsonValueCodec[LwC] =
    new JsonValueCodec[LwC] {
      override def decodeValue(in: JsonReader, default: LwC): LwC = {
        val state: LWWRegister[TaskData] = codecLwwState.decodeValue(in, default.state)
        new DeltaBufferRDT[LWWRegister[TaskData]](state, replicaId, CausalContext.empty, List())
      }
      override def encodeValue(x: LwC, out: JsonWriter): Unit = codecLwwState.encodeValue(x.state, out)
      override def nullValue: LwC = {
        println(s"reading null")
        DeltaBufferRDT[LWWRegister[TaskData]](replicaId)
      }
    }

}
