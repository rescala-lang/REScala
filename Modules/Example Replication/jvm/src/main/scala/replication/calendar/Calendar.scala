package replication.calendar

import kofre.datatypes.AddWinsSet
import rescala.default.*
import kofre.base.{Bottom, Uid}
import kofre.dotted.Dotted
import kofre.syntax.{DeltaBuffer, ReplicaId}

case class Appointment(start: Int, end: Int)

given intMaxBottom: Bottom[Int] with {
  def empty = Int.MinValue
}

class CalendarProgram(id: Uid, synchronizationPoint: String => (=> Unit) => Unit) {

  given ReplicaId = id

  type Calendar = DeltaBuffer[Dotted[AddWinsSet[Appointment]]]

  val work     = Var[Calendar](DeltaBuffer(Dotted(AddWinsSet.empty[Appointment])))
  val vacation = Var[Calendar](DeltaBuffer(Dotted(AddWinsSet.empty[Appointment])))

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
