package reswing.reader.gui

import rescala.default._
import reswing.ReListView
import reswing.ReSwingValue

class ReListViewEx[A](
    listData: ReSwingValue[Seq[A]],
    visibleRowCount: ReSwingValue[Int] = ()
) extends ReListView[A](listData, visibleRowCount) {

  val selectedIndex: Signal[Int] = selection.changed.map(_ => selection.leadIndex.value).hold(0) // #SIG //#IF
  val selectedItem = Signal { // #SIG
    if (selectedIndex.value >= 0 && selectedIndex.value < listData.value.size)
      Some(listData.value.apply(selectedIndex.value))
    else
      None
  }
}
