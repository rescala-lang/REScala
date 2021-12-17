package todo

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import loci.transmitter.IdenticallyTransmittable
import rescala.extra.lattices.delta.CContext._
import rescala.extra.lattices.delta.Codecs._
import rescala.extra.lattices.delta.crdt.reactive
import rescala.extra.lattices.delta.crdt.reactive.{LWWRegister, RGA}
import todo.Todolist.replicaId

object Codecs {

  implicit val taskRefCodec: JsonValueCodec[TaskRef] = JsonCodecMaker.make

  implicit val codecState: JsonValueCodec[RGA.State[TaskRef, DietMapCContext]] = RGAStateCodec
  implicit val codecRGA: JsonValueCodec[RGA[TaskRef, DietMapCContext]] =
    new JsonValueCodec[RGA[TaskRef, DietMapCContext]] {
      override def decodeValue(
          in: JsonReader,
          default: RGA[TaskRef, DietMapCContext]
      ): RGA[TaskRef, DietMapCContext] = {
        val state = codecState.decodeValue(in, default.state)
        new RGA[TaskRef, DietMapCContext](state, replicaId, List())
      }
      override def encodeValue(x: RGA[TaskRef, DietMapCContext], out: JsonWriter): Unit =
        codecState.encodeValue(x.state, out)
      override def nullValue: RGA[TaskRef, DietMapCContext] = RGA[TaskRef, DietMapCContext](replicaId)
    }

  implicit val transmittableList: IdenticallyTransmittable[RGA.State[TaskRef, DietMapCContext]] =
    IdenticallyTransmittable()

  implicit val todoTaskCodec: JsonValueCodec[TaskData] = JsonCodecMaker.make

  implicit val codecLwwState: JsonValueCodec[LWWRegister.State[TaskData, DietMapCContext]] = JsonCodecMaker.make

  implicit val transmittableLWW: IdenticallyTransmittable[LWWRegister.State[TaskData, DietMapCContext]] =
    IdenticallyTransmittable()

  type LwC = LWWRegister[TaskData, DietMapCContext]
  implicit val codecLww: JsonValueCodec[LwC] =
    new JsonValueCodec[LwC] {
      override def decodeValue(in: JsonReader, default: LwC): LwC = {
        val state: reactive.LWWRegister.State[TaskData, DietMapCContext] = codecLwwState.decodeValue(in, default.state)
        new LWWRegister[TaskData, DietMapCContext](state, replicaId, List())
      }
      override def encodeValue(x: LwC, out: JsonWriter): Unit = codecLwwState.encodeValue(x.state, out)
      override def nullValue: LwC = {
        println(s"reading null")
        LWWRegister[TaskData, DietMapCContext](replicaId)
      }
    }

}
