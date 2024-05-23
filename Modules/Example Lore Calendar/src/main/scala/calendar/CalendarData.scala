package calendar

import org.scalajs.dom.HTMLElement
import org.scalajs.dom.html.{Button, Div, Input, LI}
import reactives.extra.Tags.reattach
import reactives.operator.Event
import reactives.operator.Event.CBR
import scalatags.JsDom
import scalatags.JsDom.all.*
import reactives.default.*

case class Appointment(name: String, start: Int, end: Int) {
  def days: Int = end - start

  private val removeButton: CBR[Any, Button] = Event.fromCallback(button("Remove", onclick := Event.handle).render)
  private val editButton: CBR[Any, Button] = Event.fromCallback(button("Edit", onclick := Event.handle).render)

  private val nameInput: Input = input(value := name).render

  private val startInput: Input = input(value := start, `type` := "number").render
  private val endInput: Input = input(value := end, `type` := "number").render

  private val commitChange: CBR[Any, Button] = Event.fromCallback(button("Commit", onclick := Event.handle).render)
  private val currentlyEditing = (editButton.event || commitChange.event).fold(false)((s, _) => !s)

  val removeEvent: Event[Appointment] = removeButton.event.map { _ => this }
  val editEvent: Event[(Appointment, Appointment)] = commitChange.event.map { _ =>
    (this, Appointment(nameInput.value, startInput.value.toInt, endInput.value.toInt))
  }

  val tagSignal: Signal[LI] = currentlyEditing.map { c =>
    if (!c) li(span(s"$name: $start -> $end").render, br().render, removeButton.data, editButton.data).render
    else li(nameInput, span(": ").render, startInput, span(" -> ").render, endInput, commitChange.data).render
  }

  def toTag: Div =
    div().render.reattach(tagSignal, true)
}

type Calendar = Set[Appointment]
