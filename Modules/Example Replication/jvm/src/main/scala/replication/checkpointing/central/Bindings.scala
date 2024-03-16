package replication.checkpointing.central

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import rdts.datatypes.contextual.ReplicatedSet
import rdts.dotted.Dotted
import loci.registry.Binding
import loci.transmitter.IdenticallyTransmittable
import loci.serializer.jsoniterScala._
import replication.JsoniterCodecs.given

import scala.concurrent.Future

object Bindings {

  type SetState = Dotted[ReplicatedSet[Int]]

  case class SyncMessage(cp: Int, deltaState: SetState)

  case class CheckpointMessage(cp: Int, apply: List[SetState], keep: SetState)

  given IntCodec: JsonValueCodec[Int] = JsonCodecMaker.make

  given BooleanCodec: JsonValueCodec[Boolean] = JsonCodecMaker.make

  given SyncMessageCodec: JsonValueCodec[SyncMessage] = JsonCodecMaker.make

  given CheckpointMessageCodec: JsonValueCodec[CheckpointMessage] = JsonCodecMaker.make

  given transmittableSyncMessage: IdenticallyTransmittable[SyncMessage] = IdenticallyTransmittable()

  given transmittableCheckpointMessage: IdenticallyTransmittable[CheckpointMessage] = IdenticallyTransmittable()

  val receiveSyncMessageBinding: Binding[SyncMessage => Unit, SyncMessage => Future[Unit]] =
    Binding[SyncMessage => Unit]("receiveDelta")

  val assessCheckpointBinding: Binding[SyncMessage => CheckpointMessage, SyncMessage => Future[CheckpointMessage]] =
    Binding[SyncMessage => CheckpointMessage]("assessCheckpoint")

  val isCheckpointerBinding: Binding[() => Boolean, () => Future[Boolean]] = Binding[() => Boolean]("isCheckpointer")
}
