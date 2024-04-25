package calendar

import org.scalajs.dom.html.LI
import scalatags.JsDom.all.*

import scala.scalajs.js.Date


case class Appointment(name: String, start: Date, end: Date) {
  def days: Int = ???

  def toTag: LI = li(span(s"Name: $name"), br(), span(s"Start: $start"), br(), span(s"End: $end")).render
}

type Calendar = Set[Appointment]
