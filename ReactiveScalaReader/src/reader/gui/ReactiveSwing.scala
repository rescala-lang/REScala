package reader.gui

import scala.swing._
import scala.events._
import scala.events.behaviour._
import scala.swing.event.SelectionChanged
import scala.swing.event.ButtonClicked

object ReactiveSwingConversions {
    implicit def eventButtonToEvent(btn: EventButton): scala.events.Event[Button] = btn.pressed
    implicit def eventCheckBoxToEvent(check: EventCheckBox): scala.events.Event[Boolean] = check.switched
}

class EventButton(text: String) extends Button(text) {
  val pressed = new ImperativeEvent[Button]

  listenTo(this);
  reactions += { case ButtonClicked(_) => pressed(this) }
}

class EventCheckBox(text: String) extends CheckBox(text) {
  val switched    = new ImperativeEvent[Boolean]

  val activated   = switched && { _ => selected  }
  val deactivated = switched && { _ => !selected }

  listenTo(this);
  reactions += { case _ => switched(selected) }
}

class EventListView[A](evt: Event[Iterable[A]]) extends ListView[A] {
  val selectedItemChanged = new ImperativeEvent[Option[A]]

  val wrappedEvent = evt
  evt.last(1).getValue.foreach { x => listData = x.toSeq }

  evt += { data: Iterable[A] => listData = data.toSeq }

  private var selectedItemField: Option[A] = getSelectedItem

  def selectedItem: Option[A] = selectedItemField

  def selectedItem_=(item: Option[A]) = {
    selectedItemField = item
    selectedItemChanged(item)
  }

  private def getSelectedItem: Option[A] = {
    val i = selection.leadIndex
    if (i >= 0 && listData.size - 1 >= i) Some(listData(i)) else None
  }

  override def listData_=(items: Seq[A]) {
    val selected: Option[Int] = selection.indices.headOption

    super.listData = items

    selected.map { i => if (listData.size > i) { selectIndices(i) } }
  }

  listenTo(selection)
  reactions += {
    case SelectionChanged(_) => selectedItem = getSelectedItem
  }
}

class EventText[A](evt: Event[A]) extends Label {
  evt.last(1).getValue.foreach { x => text = x.toString }

  val wrappedEvent = evt
  evt += { v: A => text = v.toString }
}
