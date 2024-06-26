package replication.calendar

import rdts.base.{Bottom, LocalUid, Uid}
import rdts.datatypes.contextual.ReplicatedSet
import rdts.dotted.Dotted
import rdts.syntax.DeltaBuffer
import reactives.default.*

case class Appointment(start: Int, end: Int)

given intMaxBottom: Bottom[Int] with {
  def empty = Int.MinValue
}

class CalendarProgram(id: Uid, synchronizationPoint: String => (=> Unit) => Unit) {

  given LocalUid = id.convert

  type Calendar = DeltaBuffer[Dotted[ReplicatedSet[Appointment]]]

  val work     = Var[Calendar](DeltaBuffer(Dotted(ReplicatedSet.empty[Appointment])))
  val vacation = Var[Calendar](DeltaBuffer(Dotted(ReplicatedSet.empty[Appointment])))

  val replicated = Map("work" -> work, "vacation" -> vacation)

  val all_appointments   = Signal { work.value.data.elements.union(vacation.value.data.elements) }
  val vacation_days: Int = 30
  val remaining_vacation = Signal { vacation_days - vacation.value.data.elements.size }

  def add_appointment(calendar: Var[Calendar], appointment: Appointment): Unit =
    calendar.transform(_.modd(_.add(appointment)))
  def remove_appointment(calendar: Var[Calendar], appointment: Appointment): Unit = {
    synchronizationPoint("remove") { calendar.transform(_.modd(_.remove(appointment))) }
  }
  def change_time(
      calendar: Var[Calendar],
      appointment: Appointment,
      start: Int,
      end: Int
  ): Unit = calendar.transform { cal =>
    cal.modd(_.remove(appointment)).modd(_.add(appointment.copy(start, end)))
  }
}
