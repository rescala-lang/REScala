package reader.gui

import react.events._
import scala.swing.Button
import scala.swing.CheckBox
import scala.swing.Label
import scala.swing.ListView
import scala.swing.event.ButtonClicked
import scala.swing.event.SelectionChanged

class EventButton(text: String) extends Button(text) {
  val pressed = new ImperativeEvent[Button]  //#EVT
  reactions += { case ButtonClicked(_) => pressed(this) }  //#HDL
}

class EventCheckBox(text: String) extends CheckBox(text) {
  val switched = new ImperativeEvent[Boolean]  //#EVT
  
  val activated = switched && { _ => selected  }  //#EVT //#EF
  val deactivated = switched && { _ => !selected }  //#EVT //#EF
  
  reactions += { case _ => switched(selected) }
}

class EventListView[A](evt: Event[Iterable[A]]) extends ListView[A] {
  val selectedItemChanged = new ImperativeEvent[Option[A]]  //#EVT
  
  evt += { data => listData = data.toSeq }  //#HDL
  
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
  evt += { v => text = v.toString } //#HDL
}
