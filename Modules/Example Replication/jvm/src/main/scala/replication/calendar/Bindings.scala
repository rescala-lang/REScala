package replication.calendar

import loci.registry.Binding
import loci.transmitter.IdenticallyTransmittable
import loci.serializer.jsoniterScala.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import com.github.plokhotnyuk.jsoniter_scala.core.*
import kofre.datatypes.contextual.ReplicatedSet
import kofre.datatypes.experiments.RaftState
import kofre.dotted.Dotted
import replication.JsoniterCodecs

import scala.concurrent.Future

sealed trait SyncMessage
object SyncMessage {
  type CalendarState = Dotted[ReplicatedSet[Appointment]]

  case class AppointmentMessage(state: CalendarState, target: String) extends SyncMessage
  case class RaftMessage(state: RaftState[Token])                     extends SyncMessage
  case class WantMessage(state: Dotted[ReplicatedSet[Token]])            extends SyncMessage
  case class FreeMessage(state: Dotted[ReplicatedSet[Token]])            extends SyncMessage

}

object Bindings {

  import JsoniterCodecs.given

  implicit val AppointmentCodec: JsonValueCodec[Appointment] = JsonCodecMaker.make
  implicit val TokenCodec: JsonValueCodec[Token]             = JsonCodecMaker.make
  // implicit val RaftCodec: JsonValueCodec[RaftState[Token]] = JsonCodecMaker.make
  //
  // implicit val RaftMessageCodec: JsonValueCodec[RaftMessage] = JsonCodecMaker.make
  // implicit val WantMessageCodec: JsonValueCodec[WantMessage] = JsonCodecMaker.make
  // implicit val FreeMessageCodec: JsonValueCodec[FreeMessage] = JsonCodecMaker.make
  // implicit val AppointmentMessageCodec: JsonValueCodec[AppointmentMessage] = JsonCodecMaker.make

  implicit val SyncMessageCodec: JsonValueCodec[SyncMessage] = JsonCodecMaker.make

  implicit val transmittableSyncMessage: IdenticallyTransmittable[SyncMessage] = IdenticallyTransmittable()

  val receiveSyncMessageBinding: Binding[SyncMessage => Unit, SyncMessage => Future[Unit]] =
    Binding[SyncMessage => Unit]("receiveDelta")

}
