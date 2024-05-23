package calendar

import rdts.datatypes.contextual.{ReplicatedList, ReplicatedSet}
import org.scalajs.dom.html.{Div, Form, Input, LI}
import org.scalajs.dom
import org.scalajs.dom.{HTMLDivElement, HTMLFormElement, KeyboardEvent, UIEvent, console, document, window}
import reactives.default.*
import reactives.extra.Tags.*
import scalatags.JsDom
import scalatags.JsDom.all.*
import scalatags.JsDom.tags2.section
import scalatags.JsDom.{Attr, TypedTag, all}
import rdts.dotted.Dotted
import rdts.syntax.{DeltaBuffer, LocalUid}
import reactives.structure.Pulse
import calendar.Codecs.given
import rdts.base.Uid
import lore.dsl.{BoundInteraction, Ex, Interaction, Invariant}
import reactives.operator.Event.CBR

import javax.swing.text.html.FormSubmitEvent
import scala.annotation.targetName
import scala.scalajs.js.Date

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

  val form: Div = all.div(
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
    val newWorkAppointment = new NewAppointment("Work")
    val newVacation        = new NewAppointment("Vacation")

    // ====================== RDTs ====================== //

    val workCalendarRDT: Var[Calendar] =
      Var(DeltaBuffer(Dotted(ReplicatedSet.empty[Appointment])))

    val vacationCalendarRDT: Var[Calendar] =
      Var(DeltaBuffer(Dotted(ReplicatedSet.empty[Appointment])))

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
            if (a != b) {
              a.end < b.start || a.start > b.end
            } else true
          }
        }
      }

    noAppointmentsCrossing(workCalendarRDT, workCalendarRDT)
    noAppointmentsCrossing(vacationCalendarRDT, workCalendarRDT)
    noAppointmentsCrossing(vacationCalendarRDT, vacationCalendarRDT)

    // ====================== add ====================== //

    val addAppointment = Interaction[Calendar, Appointment]
      .requires { (_, a: Appointment) => a.start <= a.end }
      .requires { (cal: Calendar, a) => !cal.contains(a) }
      .executes { (cal: Calendar, a) => cal.clearDeltas().add(a) }
      .ensures { (cal: Calendar, a) => cal.contains(a) }

    val addWorkAppointment = addAppointment
      .modifies(workCalendarRDT)
      .actsOn(newWorkAppointment.submitEvent)

    val addVacationAppointment = addAppointment
      .modifies(vacationCalendarRDT)
      .actsOn(newVacation.submitEvent)

    // ====================== remove ====================== //

    val removeAppointment = Interaction[Calendar, Appointment]
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
      cal.map { buf => buf.elements.toList.map(_.removeEvent) }.flatten(Flatten.firstFiringEvent)

    val removeWorkAppointment = removeAppointment
      .modifies(workCalendarRDT)
      .actsOn(removeEvents(workCalendarRDT))

    val removeVacationAppointment = removeAppointment
      .modifies(vacationCalendarRDT)
      .actsOn(removeEvents(vacationCalendarRDT))

    /*
    val workCalendarRDT: Signal[Calendar] =
      Storing.storedAs(storagePrefix, DeltaBuffer(Dotted(ReplicatedSet.empty[Appointment]))) { init =>
        Fold(init)(
          newWorkAppointment.submitEvent act { (a: Appointment) =>
            current.clearDeltas().add(a)
          }
        )
      }
     */

    // ====================== edit ====================== //

    val changeAppointment = Interaction[Calendar, (Appointment, Appointment)]
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
      cal.map { buf => buf.elements.toList.map(_.editEvent) }.flatten(Flatten.firstFiringEvent)

    val ewe = editEvents(workCalendarRDT)

    ewe.observe { it => println(s"Called edit with $it") }

    val editWorkAppointment = changeAppointment
      .modifies(workCalendarRDT)
      .actsOn(ewe)

    val editVacationAppointment = changeAppointment
      .modifies(vacationCalendarRDT)
      .actsOn(editEvents(vacationCalendarRDT))

    // ====================== Publishing ====================== //

    GlobalRegistry.publish("workCal", workCalendarRDT)

    // ====================== UI ====================== //

    val workCalList: Signal[List[Appointment]] = workCalendarRDT.map { it => it.elements.toList.sortBy(_.start) }
    val workCalTags: Signal[List[Div]]         = workCalList.map { it => it.map { _.toTag } }

    val vacationCalList: Signal[List[Appointment]] = vacationCalendarRDT.map { it => it.elements.toList.sortBy(_.start) }
    val vacationCalTags: Signal[List[Div]]         = vacationCalList.map { it => it.map { _.toTag } }

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
