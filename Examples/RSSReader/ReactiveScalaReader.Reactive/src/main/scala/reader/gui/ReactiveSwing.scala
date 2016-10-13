package reader.gui

import rescala._
import reswing.ReListView
import reswing.ReSwingValue

class ReListViewEx[A](
    listData: ReSwingValue[Seq[A]],
    visibleRowCount: ReSwingValue[Int] = ())
  extends
    ReListView[A](listData, visibleRowCount) {

  val selectedIndex = selection.changed.snapshot(selection.leadIndex) //#SIG //#IF
  val selectedItem = Signal { //#SIG
    if (selectedIndex() >= 0 && selectedIndex() < listData.apply().size)
      Some(listData.apply().apply(selectedIndex()))
    else
      None
  }
}
