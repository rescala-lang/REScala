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
import lore.dsl.{BoundInteraction, Interaction}
import lore.dsl.Ex

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
    `type`  := "date"
  ).render

  val dateEndInput: Input = input(
    `class` := "new-appointment-date-end",
    `type`  := "date"
  ).render

  val submitButton = Event.fromCallback(input(
    `class` := "new-appointment-submit",
    `type`  := "button",
    onclick := Event.handle,
    value := "Submit"
  ).render)

  val form = all.div(
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
    val name = nameInput.value
    val dateStart = new Date(dateStartInput.value)
    val dateEnd = new Date(dateEndInput.value)

    console.log(dateStartInput.value, dateStart)

    nameInput.value = ""
    dateStartInput.value = ""
    dateEndInput.value = ""

    Appointment(name, dateStart, dateEnd)
  }

}

class CalendarUI(val storagePrefix: String, val replicaId: Uid) {

  given LocalUid = replicaId

  def getContents(): Div = {
    val newWorkAppointment = new NewAppointment("Work")
    val newVacation        = new NewAppointment("Vacation")

    val workCalendarRDT: Var[DeltaBuffer[Dotted[ReplicatedSet[Appointment]]]] =
      Var(DeltaBuffer(Dotted(ReplicatedSet.empty[Appointment])))

    val addInteraction = Interaction[DeltaBuffer[Dotted[ReplicatedSet[Appointment]]], Appointment]
      .requires { (_, a: Appointment) => a.start.getDate() < a.end.getDate() }
      .requires { (cal: DeltaBuffer[Dotted[ReplicatedSet[Appointment]]], a) => !cal.contains(a) }
      .executes { (cal: DeltaBuffer[Dotted[ReplicatedSet[Appointment]]], a) => cal.clearDeltas().add(a) }
      .ensures { (cal: DeltaBuffer[Dotted[ReplicatedSet[Appointment]]], a) => cal.contains(a) }
      .modifies(workCalendarRDT)

    newWorkAppointment.submitEvent.observe { it =>
      addInteraction.apply(it)
    }

    /*
    val workCalendarRDT: Signal[DeltaBuffer[Dotted[ReplicatedSet[Appointment]]]] =
      Storing.storedAs(storagePrefix, DeltaBuffer(Dotted(ReplicatedSet.empty[Appointment]))) { init =>
        Fold(init)(
          newWorkAppointment.submitEvent act { (a: Appointment) =>
            current.clearDeltas().add(a)
          }
        )
      }
     */

    GlobalRegistry.publish("workCal", workCalendarRDT)

    val workCalList                   = workCalendarRDT.map { it => it.elements.toList }
    val workCalTags: Signal[List[LI]] = workCalList.map { it => it.map { _.toTag } }

    div(
      h1("LoRe Calendar"),
      div(
        id := "work-appointment-container",
        newWorkAppointment.form,
        ul().render.reattach(workCalTags)
      ),
      div(
        id := "vacation-container",
        newVacation.form
      )
    ).render
  }

}
