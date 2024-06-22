package replication.calendar

import rdts.datatypes.contextual.ReplicatedSet
import rdts.datatypes.experiments.protocols.raft.{RaftState, RaftToken}
import rdts.dotted.Dotted

sealed trait SyncMessage
object SyncMessage {
  type CalendarState = Dotted[ReplicatedSet[Appointment]]

  case class AppointmentMessage(state: CalendarState, target: String) extends SyncMessage
  case class RaftMessage(state: RaftState[RaftToken])                 extends SyncMessage
  case class WantMessage(state: Dotted[ReplicatedSet[RaftToken]])     extends SyncMessage
  case class FreeMessage(state: Dotted[ReplicatedSet[RaftToken]])     extends SyncMessage

}
