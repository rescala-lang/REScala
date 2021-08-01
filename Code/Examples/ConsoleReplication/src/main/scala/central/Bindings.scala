package central

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import loci.registry.Binding
import loci.transmitter.transmittable.IdenticallyTransmittable
import loci.serializer.jsoniterScala._
import rescala.extra.lattices.delta.CContext.DietMapCContext
import rescala.extra.lattices.delta.crdt.reactive.AWSet
import rescala.extra.lattices.delta.Codecs._

import scala.concurrent.Future

object Bindings {

  type SetState = AWSet.State[Int, DietMapCContext]

  case class SyncMessage(cp: Int, deltaState: SetState)

  case class CheckpointMessage(cp: Int, apply: List[SetState], keep: SetState)

  implicit val IntCodec: JsonValueCodec[Int] = JsonCodecMaker.make

  implicit val BooleanCodec: JsonValueCodec[Boolean] = JsonCodecMaker.make

  implicit val SyncMessageCodec: JsonValueCodec[SyncMessage] = JsonCodecMaker.make

  implicit val CheckpointMessageCodec: JsonValueCodec[CheckpointMessage] = JsonCodecMaker.make

  implicit val transmittableSyncMessage: IdenticallyTransmittable[SyncMessage] = IdenticallyTransmittable()

  implicit val transmittableCheckpointMessage: IdenticallyTransmittable[CheckpointMessage] = IdenticallyTransmittable()

  val receiveSyncMessageBinding: Binding[SyncMessage => Unit, SyncMessage => Future[Unit]] =
    Binding[SyncMessage => Unit]("receiveDelta")

  val assessCheckpointBinding: Binding[SyncMessage => CheckpointMessage, SyncMessage => Future[CheckpointMessage]] =
    Binding[SyncMessage => CheckpointMessage]("assessCheckpoint")

  val isCheckpointerBinding: Binding[() => Boolean, () => Future[Boolean]] = Binding[() => Boolean]("isCheckpointer")
}
