package todo

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonValueCodec, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import kofre.causality.CausalContext
import loci.transmitter.IdenticallyTransmittable
import rescala.extra.lattices.delta.JsoniterCodecs._
import rescala.extra.lattices.delta.crdt.reactive
import rescala.extra.lattices.delta.crdt.reactive.{LWWRegister, RGA}
import todo.Todolist.replicaId

object Codecs {

  implicit val taskRefCodec: JsonValueCodec[TaskRef] = JsonCodecMaker.make

  implicit val codecState: JsonValueCodec[RGA.State[TaskRef, CausalContext]] = RGAStateCodec
  implicit val codecRGA: JsonValueCodec[RGA[TaskRef, CausalContext]]         =
    new JsonValueCodec[RGA[TaskRef, CausalContext]] {
      override def decodeValue(
          in: JsonReader,
          default: RGA[TaskRef, CausalContext]
      ): RGA[TaskRef, CausalContext] = {
        val state = codecState.decodeValue(in, default.state)
        new RGA[TaskRef, CausalContext](state, replicaId, List())
      }
      override def encodeValue(x: RGA[TaskRef, CausalContext], out: JsonWriter): Unit =
        codecState.encodeValue(x.state, out)
      override def nullValue: RGA[TaskRef, CausalContext] = RGA[TaskRef, CausalContext](replicaId)
    }

  implicit val transmittableList: IdenticallyTransmittable[RGA.State[TaskRef, CausalContext]] =
    IdenticallyTransmittable()

  implicit val todoTaskCodec: JsonValueCodec[TaskData] = JsonCodecMaker.make

  implicit val codecLwwState: JsonValueCodec[LWWRegister.State[TaskData, CausalContext]] = JsonCodecMaker.make

  implicit val transmittableLWW: IdenticallyTransmittable[LWWRegister.State[TaskData, CausalContext]] =
    IdenticallyTransmittable()

  type LwC = LWWRegister[TaskData, CausalContext]
  implicit val codecLww: JsonValueCodec[LwC] =
    new JsonValueCodec[LwC] {
      override def decodeValue(in: JsonReader, default: LwC): LwC = {
        val state: reactive.LWWRegister.State[TaskData, CausalContext] = codecLwwState.decodeValue(in, default.state)
        new LWWRegister[TaskData, CausalContext](state, replicaId, List())
      }
      override def encodeValue(x: LwC, out: JsonWriter): Unit = codecLwwState.encodeValue(x.state, out)
      override def nullValue: LwC = {
        println(s"reading null")
        LWWRegister[TaskData, CausalContext](replicaId)
      }
    }

}
