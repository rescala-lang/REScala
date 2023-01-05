package replication.calendar

import kofre.datatypes.AddWinsSet
import rescala.default._
import kofre.datatypes.AddWinsSet.AWSetSyntax
import kofre.decompose.containers.DeltaBufferRDT
import kofre.base.Id

case class Appointment(start: Int, end: Int)

class CalendarProgram(id: Id, synchronizationPoint: String => (=> Unit) => Unit) {

  type Calendar = DeltaBufferRDT[AddWinsSet[Appointment]]

  val work     = Var[Calendar](DeltaBufferRDT(id, AddWinsSet.empty[Appointment]))
  val vacation = Var[Calendar](DeltaBufferRDT(id, AddWinsSet.empty[Appointment]))

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
