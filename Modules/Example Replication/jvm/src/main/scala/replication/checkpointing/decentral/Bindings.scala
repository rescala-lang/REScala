package replication.checkpointing.decentral

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import kofre.dotted.Dotted
import loci.registry.Binding
import loci.serializer.jsoniterScala.{given, _}
import loci.transmitter.IdenticallyTransmittable
import replication.JsoniterCodecs.{given, _}
import kofre.base.Uid
import kofre.datatypes.contextual.AddWinsSet

import scala.concurrent.Future

object Bindings {
  type SetState = Dotted[AddWinsSet[Int]]

  case class CheckpointMessage(cp: Checkpoint, changes: SetState)

  implicit val intCodec: JsonValueCodec[Int] = JsonCodecMaker.make

  implicit val checkpointCodec: JsonValueCodec[Checkpoint] = JsonCodecMaker.make

  implicit val setStateMessageCodec: JsonValueCodec[SetState] = JsonCodecMaker.make
  given JsonValueCodec[Map[Uid, Int]]                         = JsonCodecMaker.make

  implicit val checkpointMessageCodec: JsonValueCodec[CheckpointMessage] = JsonCodecMaker.make

  implicit val transmittableSetState: IdenticallyTransmittable[SetState] = IdenticallyTransmittable()
  given IdenticallyTransmittable[Map[Uid, Int]]                          = IdenticallyTransmittable()

  implicit val transmittableCheckpointMessage: IdenticallyTransmittable[CheckpointMessage] = IdenticallyTransmittable()

  val receiveDeltaBinding: Binding[SetState => Unit, SetState => Future[Unit]] =
    Binding[SetState => Unit]("receiveDelta")

  val getCheckpointsBinding: Binding[() => Map[Uid, Int], () => Future[Map[Uid, Int]]] =
    Binding[() => Map[Uid, Int]]("getCheckpoints")

  val receiveCheckpointBinding: Binding[CheckpointMessage => Unit, CheckpointMessage => Future[Unit]] =
    Binding[CheckpointMessage => Unit]("receiveCheckpoint")
}
