package reader.gui

import scala.swing.ListView
import rescala.events.ImperativeEvent
import reswing.ReListView
import reswing.ReSwingValue

class ReListViewEx[A](
    listData: ReSwingValue[Seq[A]],
    visibleRowCount: ReSwingValue[Int] = ())
  extends
    ReListView[A](listData, visibleRowCount) {
  
  val selectedItemChanged = new ImperativeEvent[Option[A]]  //#EVT
  
  private var selectedItemField: Option[A] = getSelectedItem
  
  def selectedItem: Option[A] = selectedItemField
  
  def selectedItem_=(item: Option[A]) = {
    selectedItemField = item
    selectedItemChanged(item)
    println(item);
  }
  
  private def getSelectedItem: Option[A] = {
    val listView = (this: ListView[A])
    val i = listView.selection.leadIndex
    if (i >= 0 && i < listView.listData.size) Some(listView.listData(i)) else None
  }
  
  selection.changed += { _ => selectedItem = getSelectedItem }
}

