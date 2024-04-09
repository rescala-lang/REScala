package lore.calendar

import lore.dsl.*
import reactives.default.{Signal as Derived, Var as Source}

import scala.collection.immutable.Set as AWSet

trait Appointment {
  def start: Int

  def end: Int

  def days: Int
}

type Calendar = AWSet[Appointment]

def sumDays(cal: Calendar): Int = {
  ???
}

val work: Source[Calendar] = Source(AWSet())
val vacation: Source[Calendar] = Source(AWSet())

val all_appointments: Derived[Set[Appointment]] = Derived {
  work.value.union(vacation.value)
}

val remaining_vacation: Derived[Int] = Derived {
  30 - sumDays(vacation.value)
}

val add_appointment =
  Interaction[Calendar, Appointment]
    .requires { (_, a) => a.start < a.end }
    .requires { (cal: Calendar, a) => !cal.contains(a) }
    .executes { (cal: Calendar, a) => cal + a }
    .ensures { (cal: Calendar, a) => cal.contains(a) }

val add_vacation = add_appointment.modifies(vacation)
  .requires { (cal, a) => remaining_vacation.value - a.days >= 0 }

// UI.display(all_appointments, remaining_vacation)
// UI.vacationDialog.onConfirm { a => add_vacation.apply(a) }



def main(args: Array[String]): Unit = {
  Invariant {
    all_appointments.value.forall { a => a.start < a.end }
  }
}
