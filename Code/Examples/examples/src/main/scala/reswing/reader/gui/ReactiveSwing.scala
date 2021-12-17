package reswing.reader.gui

import rescala.default._
import reswing.ReListView
import reswing.ReSwingValue

class ReListViewEx[A](
    listData: ReSwingValue[Seq[A]],
    visibleRowCount: ReSwingValue[Int] = ()
) extends ReListView[A](listData, visibleRowCount) {

  val selectedIndex: Signal[Int] = selection.changed.map(_ => selection.leadIndex()).latest(0) // #SIG //#IF
  val selectedItem = Signal { // #SIG
    if (selectedIndex() >= 0 && selectedIndex() < listData.apply().size)
      Some(listData.apply().apply(selectedIndex()))
    else
      None
  }
}
