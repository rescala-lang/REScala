package reader.gui

import scala.events.Event
import scala.events.ImperativeEvent
import scala.swing.Button
import scala.swing.CheckBox
import scala.swing.Label
import scala.swing.ListView
import scala.swing.event.ButtonClicked
import scala.swing.event.SelectionChanged

class EventButton(text: String) extends Button(text) {
  val pressed = new ImperativeEvent[Button]
  reactions += { case ButtonClicked(_) => pressed(this) }
}

class EventCheckBox(text: String) extends CheckBox(text) {
  val switched = new ImperativeEvent[Boolean]
  
  val activated = switched && { _ => selected  }
  val deactivated = switched && { _ => !selected }
  
  reactions += { case _ => switched(selected) }
}

class EventListView[A](evt: Event[Iterable[A]]) extends ListView[A] {
  val selectedItemChanged = new ImperativeEvent[Option[A]]
  
  evt += { data => listData = data.toSeq }
  
  private var selectedItemField: Option[A] = getSelectedItem
  
  def selectedItem: Option[A] = selectedItemField
  
  def selectedItem_=(item: Option[A]) = {
    selectedItemField = item
    selectedItemChanged(item)
  }
  
  private def getSelectedItem: Option[A] = {
    val i = selection.leadIndex
    if (i >= 0 && i < listData.size) Some(listData(i)) else None
  }
  
  override def listData_=(items: Seq[A]) {
    val selected = selection.indices.headOption
    super.listData = items
    for (index <- selected)
      selectIndices(index)
  }

  listenTo(selection)
  
  reactions += {
    case SelectionChanged(_) => selectedItem = getSelectedItem
  }
}

class EventText[A](evt: Event[A]) extends Label {
  evt += { v => text = v.toString }
}
