package reader.gui

import makro.SignalMacro.{SignalM => Signal}
import reswing.ReListView
import reswing.ReSwingValue

class ReListViewEx[A](
    listData: ReSwingValue[Seq[A]],
    visibleRowCount: ReSwingValue[Int] = ())
  extends
    ReListView[A](listData, visibleRowCount) {
  
  val selectedIndex = selection.leadIndex snapshot selection.changed //#SIG //#IF
  val selectedItem = Signal { //#SIG
    if (selectedIndex() >= 0 && selectedIndex() < listData().size)
      Some(listData()(selectedIndex()))
    else
      None
  }
}
