package calendar

import calendar.Codecs.given
import lore.dsl.{Interaction, InteractionWithExecutes, Invariant}
import org.scalajs.dom.html.{Div, Input}
import rdts.base.Uid
import rdts.datatypes.contextual.ReplicatedSet
import rdts.dotted.Dotted
import rdts.syntax.{DeltaBuffer, LocalUid}
import reactives.core.CreationTicket
import reactives.default.*
import reactives.extra.Tags.*
import reactives.operator.Event.CBR
import replication.Storing
import scalatags.JsDom.all.*

class NewAppointment(private val typeName: String) {

  val nameInput: Input = input(
    `class`     := "new-appointment-name",
    placeholder := "Appointment Name",
    `type`      := "text",
  ).render

  val dateStartInput: Input = input(
    `class` := "new-appointment-date-start",
    `type`  := "number"
  ).render

  val dateEndInput: Input = input(
    `class` := "new-appointment-date-end",
    `type`  := "number"
  ).render

  val submitButton: CBR[Any, Input] = Event.fromCallback(input(
    `class` := "new-appointment-submit",
    `type`  := "button",
    onclick := Event.handle,
    value   := "Submit"
  ).render)

  val form: Div = div(
    `class` := "new-appointment-form",
    h2(
      `class` := "new-appointment-title",
      s"Create new $typeName Appointment"
    ),
    span("Appointment Name:"),
    nameInput,
    span("Appointment Start Date:"),
    dateStartInput,
    span("Appointment End Date:"),
    dateEndInput,
    submitButton.data,
  ).render

  val submitEvent: Event[Appointment] = submitButton.event.map { _ =>
    val name      = nameInput.value
    val dateStart = dateStartInput.value.toInt
    val dateEnd   = dateEndInput.value.toInt

    nameInput.value = ""
    dateStartInput.value = ""
    dateEndInput.value = ""

    Appointment(name, dateStart, dateEnd)
  }

}

class CalendarUI(val storagePrefix: String, val replicaId: Uid) {

  given LocalUid = replicaId

  type Calendar = DeltaBuffer[Dotted[ReplicatedSet[Appointment]]]

  def getContents(): Div = {
    // ====================== Inputs ====================== //

    val newWorkAppointment = new NewAppointment("Work")
    val newVacation        = new NewAppointment("Vacation")

    // ====================== Invariants ====================== //

    val noAppointmentsCrossing = (
        c1: Var[Calendar],
        c2: Var[Calendar]
    ) =>
      Invariant {
        val cal1 = c1.value
        val cal2 = c2.value

        cal1.elements.forall { a =>
          cal2.elements.forall { b =>
            if a != b then {
              a.end < b.start || a.start > b.end
            } else true
          }
        }
      }

    /*
    noAppointmentsCrossing(workCalendarRDT, workCalendarRDT)
    noAppointmentsCrossing(vacationCalendarRDT, workCalendarRDT)
    noAppointmentsCrossing(vacationCalendarRDT, vacationCalendarRDT)
     */

    // ====================== add ====================== //

    val addAppointment = Interaction[Calendar, Appointment]
      .requires { (_, a: Appointment) => a.start <= a.end }
      .requires { (cal: Calendar, a) => !cal.contains(a) }
      .executes { (cal: Calendar, a) => cal.clearDeltas().add(a) }
      .ensures { (cal: Calendar, a) => cal.contains(a) }

    // ====================== remove ====================== //

    val removeAppointment: InteractionWithExecutes[Tuple1[Calendar], Appointment] = Interaction[Calendar, Appointment]
      .requires { (cal: Calendar, a) => cal.contains(a) }
      .executes { (cal: Calendar, a) => cal.clearDeltas().remove(a) }
      .ensures { (cal: Calendar, a) => !cal.contains(a) }
    /*
      .ensures { (cal: Calendar, a) =>
        cal == old(cal).setminus(Set(a))
      }
      .ensures { (cal: Calendar, a) =>
        sumDays(cal) == old(sumDays(cal.toSet)) - sumDays(Set(a))
      }
     */

    val removeEvents = (cal: Signal[Calendar]) =>
      cal.map { buf => buf.elements.toList.map(_.removeEvent) }.flatten(using Flatten.firstFiringEvent)

    // ====================== edit ====================== //

    val editAppointment = Interaction[Calendar, (Appointment, Appointment)]
      .requires { (cal: Calendar, aps) => aps._2.start <= aps._2.end }
      .requires { (cal: Calendar, aps) => cal.elements.contains(aps._1) }
      .requires { (cal: Calendar, aps) => !cal.elements.contains(aps._2) }
      .executes { (cal: Calendar, aps) => cal.clearDeltas().remove(aps._1).add(aps._2) }
      .ensures { (cal: Calendar, aps) => cal.elements.contains(aps._2) }
      // .ensures { (cal: Calendar, aps) => cal.toSet == old(cal.toSet.setminus(Set(oldApp)).union(Set(newApp))) }
      .ensures { (cal: Calendar, aps) => !cal.elements.contains(aps._1) }
      .ensures { (cal: Calendar, aps) => cal.elements contains aps._2 }
    // .ensures { (cal: Calendar, aps) => size(cal.toSet) == old(size(cal.toSet)) }

    val editEvents = (cal: Signal[Calendar]) =>
      cal.map { buf => buf.elements.toList.map(_.editEvent) }.flatten(using Flatten.firstFiringEvent)

    // ====================== RDTs ====================== //

    // val deltaEvt = GlobalRegistry.subscribe[ReplicatedSet[Appointment]]("workCal")

    def events[T](cal: Calendar)(mapper: Appointment => Event[T]): Event[T] = {
      val events = cal.elements.map(mapper)

      Event.Impl.static(events.toSeq*) { st =>
        events.map(st.dependStatic)
          .collectFirst { case Some(e) => e }
      }
    }

    val workCalendarRDT: Signal[Calendar] =
      Storing.storedAs(storagePrefix + "-work", DeltaBuffer(Dotted(ReplicatedSet.empty[Appointment]))) { init =>
        Fold(init)(
          addAppointment.actsOn(newWorkAppointment.submitEvent).foldInto,
          removeAppointment.actWith[Calendar](events(current)(_.removeEvent)),
          editAppointment.actWith[Calendar](events(current)(_.editEvent)),
        )
      }

    val vacationCalendarRDT: Signal[Calendar] =
      Storing.storedAs(storagePrefix + "-vacation", DeltaBuffer(Dotted(ReplicatedSet.empty[Appointment]))) { init =>
        Fold(init)(
          addAppointment.actsOn(newVacation.submitEvent).foldInto,
          removeAppointment.actWith[Calendar](events(current)(_.removeEvent)),
          editAppointment.actWith[Calendar](events(current)(_.editEvent)),
        )
      }

    // ====================== Publishing ====================== //

    // GlobalRegistry.publish("workCal", workCalendarRDT)

    // ====================== UI ====================== //

    val workCalList: Signal[List[Appointment]] = workCalendarRDT.map { it => it.elements.toList.sortBy(_.start) }
    val workCalTags: Signal[List[Div]]         = workCalList.map { it => it.map { _.toTag } }

    val vacationCalList: Signal[List[Appointment]] = vacationCalendarRDT.map { it =>
      it.elements.toList.sortBy(_.start)
    }
    val vacationCalTags: Signal[List[Div]] = vacationCalList.map { it => it.map { _.toTag } }

    div(
      h1("LoRe Calendar"),
      div(
        id := "work-appointment-container",
        newWorkAppointment.form,
        ul().render.reattach(workCalTags)
      ),
      div(
        id := "vacation-container",
        newVacation.form,
        ul().render.reattach(vacationCalTags)
      )
    ).render
  }

}
