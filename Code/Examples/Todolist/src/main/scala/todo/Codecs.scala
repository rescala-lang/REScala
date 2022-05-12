package todo

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonKeyCodec, JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import kofre.base.Defs
import kofre.causality.Dot
import kofre.contextual.WithContext
import kofre.decompose.containers.DeltaBufferRDT
import kofre.decompose.interfaces.LWWRegisterInterface.LWWRegister
import kofre.decompose.interfaces.{LWWRegisterInterface, RGA}
import loci.transmitter.IdenticallyTransmittable
import rescala.extra.lattices.delta.JsoniterCodecs._
import todo.Todolist.replicaId

object Codecs {

  implicit val taskRefCodec: JsonValueCodec[TaskRef] = JsonCodecMaker.make
  implicit val dotKeyCodec: JsonKeyCodec[Dot]   = new JsonKeyCodec[Dot] {
    override def decodeKey(in: JsonReader): Dot = {
      val Array(time, id) = in.readKeyAsString().split("-", 2)
      Dot(id.asInstanceOf[Defs.Id], time.asInstanceOf[Defs.Time])
    }
    override def encodeKey(x: Dot, out: JsonWriter): Unit = out.writeKey(s"${x.time}-${x.replicaId}")
  }

  implicit val codecState: JsonValueCodec[WithContext[RGA[TaskRef]]]               = RGAStateCodec
  implicit val codecRGA: JsonValueCodec[DeltaBufferRDT[RGA[TaskRef]]] =
    new JsonValueCodec[DeltaBufferRDT[RGA[TaskRef]]] {
      override def decodeValue(
          in: JsonReader,
          default: DeltaBufferRDT[RGA[TaskRef]]
      ): DeltaBufferRDT[RGA[TaskRef]] = {
        val state = codecState.decodeValue(in, default.state)
        new DeltaBufferRDT[RGA[TaskRef]](state, replicaId, List())
      }
      override def encodeValue(x: DeltaBufferRDT[RGA[TaskRef]], out: JsonWriter): Unit =
        codecState.encodeValue(x.state, out)
      override def nullValue: DeltaBufferRDT[RGA[TaskRef]] = DeltaBufferRDT(replicaId, RGA.empty[TaskRef])
    }

  implicit val transmittableList: IdenticallyTransmittable[WithContext[RGA[TaskRef]]] =
    IdenticallyTransmittable()

  implicit val todoTaskCodec: JsonValueCodec[TaskData] = JsonCodecMaker.make

  implicit val codecLwwState: JsonValueCodec[WithContext[LWWRegister[TaskData]]] = JsonCodecMaker.make

  implicit val transmittableLWW: IdenticallyTransmittable[WithContext[LWWRegister[TaskData]]] =
    IdenticallyTransmittable()

  type LwC = DeltaBufferRDT[LWWRegister[TaskData]]
  implicit val codecLww: JsonValueCodec[LwC] =
    new JsonValueCodec[LwC] {
      override def decodeValue(in: JsonReader, default: LwC): LwC = {
        val state: WithContext[LWWRegister[TaskData]] = codecLwwState.decodeValue(in, default.state)
        new DeltaBufferRDT[LWWRegister[TaskData]](state, replicaId, List())
      }
      override def encodeValue(x: LwC, out: JsonWriter): Unit = codecLwwState.encodeValue(x.state, out)
      override def nullValue: LwC = {
        println(s"reading null")
        DeltaBufferRDT(replicaId, LWWRegisterInterface.empty[TaskData])
      }
    }



}
