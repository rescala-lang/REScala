package reswingexamples.reader.gui

import reactives.default.*
import reswing.{ReListView, ReSwingValue}

class ReListViewEx[A](
    listData: ReSwingValue[Seq[A]],
    visibleRowCount: ReSwingValue[Int] = ()
) extends ReListView[A](listData, visibleRowCount) {

  val selectedIndex: Signal[Int] = selection.changed.map(_ => selection.leadIndex.value).hold(0) // #SIG //#IF
  val selectedItem = Signal { // #SIG
    if selectedIndex.value >= 0 && selectedIndex.value < listData.value.size then
      Some(listData.value.apply(selectedIndex.value))
    else
      None
  }
}
