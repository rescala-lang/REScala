package reader.gui

import scala.events.ImperativeEvent
import scala.events.behaviour.Signal
import scala.events.behaviour.Var
import scala.swing.Button
import scala.swing.CheckBox
import scala.swing.Label
import scala.swing.ListView
import scala.swing.event.ButtonClicked
import scala.swing.event.SelectionChanged

object ReactiveSwingConversions {
  implicit def eventButtonToEvent(btn: ReButton): scala.events.Event[Button] = btn.pressed
  implicit def eventCheckBoxToEvent(check: ReCheckBox): scala.events.Event[Boolean] = check.switched
}

class ReButton(text: String) extends Button(text) {
  val pressed = new ImperativeEvent[Button]
  
  listenTo(this)
  reactions += { case ButtonClicked(_) => pressed(this) }
}

class ReCheckBox(text: String) extends CheckBox(text) {
  val switched = new ImperativeEvent[Boolean]
  
  val activated = switched && { _ => selected  }
  val deactivated = switched && { _ => !selected }
  
  listenTo(this)
  reactions += { case _ => switched(selected) }
}

class ReListView[A](s: Signal[Iterable[A]]) extends ListView[A] {
  private val selectedItemVar = Var[Option[A]](None)
  val selectedItem = Signal { selectedItemVar() }
  
  s.changed += { data => listData = data.toSeq }
  
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
    case SelectionChanged(_) => selectedItemVar() = getSelectedItem
  }
}

class ReText[A](s: Signal[A]) extends Label {
  s.changed += { v => text = v.toString }
}