package calendar
import kofre.predef.AddWinsSet
import rescala.default._
import kofre.predef.AddWinsSet.AWSetSyntax
import kofre.decompose.containers.ReactiveDeltaCRDT

case class Appointment(start: Int, end: Int)

class CalendarProgram(id: String, synchronizationPoint: String => (=> Unit) => Unit) {

  type Calendar = ReactiveDeltaCRDT[AddWinsSet[Appointment]]

  val work     = Var[Calendar](ReactiveDeltaCRDT(id))
  val vacation = Var[Calendar](ReactiveDeltaCRDT(id))

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
