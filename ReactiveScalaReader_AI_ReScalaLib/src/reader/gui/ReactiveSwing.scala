package reader.gui

import scala.swing.Button
import scala.swing.CheckBox
import scala.swing.Label
import scala.swing.ListView
import scala.swing.event.ButtonClicked
import scala.swing.event.SelectionChanged

import macro.SignalMacro.{SignalM => Signal}
import react.Signal
import react.SignalSynt
import react.Var
import react.events.ImperativeEvent

class ReButton(text: String) extends Button(text) {
  val pressed = new ImperativeEvent[Button]
  reactions += { case ButtonClicked(_) => pressed(this) }
}

class ReCheckBox(text: String) extends CheckBox(text) {
  private val selectedVar = Var(false)
  val checked = Signal { selectedVar() }
  
  val activated = checked.changed && (v => v)
  val deactivated = checked.changed && (! _)
  
  reactions += { case _ => selectedVar() = selected }
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