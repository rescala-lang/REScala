package replication.calendar

import rescala.default.*
import rdts.base.{Bottom, Uid}
import rdts.datatypes.contextual.ReplicatedSet
import rdts.dotted.Dotted
import rdts.syntax.{DeltaBuffer, ReplicaId}

case class Appointment(start: Int, end: Int)

given intMaxBottom: Bottom[Int] with {
  def empty = Int.MinValue
}

class CalendarProgram(id: Uid, synchronizationPoint: String => (=> Unit) => Unit) {

  given ReplicaId = id

  type Calendar = DeltaBuffer[Dotted[ReplicatedSet[Appointment]]]

  val work     = Var[Calendar](DeltaBuffer(Dotted(ReplicatedSet.empty[Appointment])))
  val vacation = Var[Calendar](DeltaBuffer(Dotted(ReplicatedSet.empty[Appointment])))

  val replicated = Map("work" -> work, "vacation" -> vacation)

  val all_appointments   = Signal { work.value.elements.union(vacation.value.elements) }
  val vacation_days: Int = 30
  val remaining_vacation = Signal { vacation_days - vacation.value.elements.size }

  def add_appointment(calendar: Var[Calendar], appointment: Appointment): Unit =
    calendar.transform(_.add(appointment))
  def remove_appointment(calendar: Var[Calendar], appointment: Appointment): Unit = {
    synchronizationPoint("remove") { calendar.transform(_.remove(appointment)) }
  }
  def change_time(
      calendar: Var[Calendar],
      appointment: Appointment,
      start: Int,
      end: Int
  ): Unit = calendar.transform(
    _.remove(appointment).add(appointment.copy(start, end))
  )
}
