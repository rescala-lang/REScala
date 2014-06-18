package reswing

import scala.language.implicitConversions
import scala.collection.JavaConverters._
import scala.swing.Color
import scala.swing.Dimension
import scala.swing.Font
import scala.swing.ListView
import scala.swing.ListView.IntervalMode
import scala.swing.event.ListSelectionChanged
import scala.swing.event.ListElementsAdded
import scala.swing.event.ListChanged
import scala.swing.event.ListElementsRemoved

class ReListView[A](
    val listData: ReSwingValue[Seq[A]] = ReSwingNoValue[Seq[A]],
    val visibleRowCount: ReSwingValue[Int] = (),
    val selectionForeground: ReSwingValue[Color] = (),
    val selectionBackground: ReSwingValue[Color] = (),
    val selectIndices: ReSwingEvent[Seq[Int]] = (),
    val ensureIndexIsVisible: ReSwingEvent[Int] = (),
    `selection.intervalMode`: ReSwingValue[IntervalMode.Value] = (),
    background: ReSwingValue[Color] = (),
    foreground: ReSwingValue[Color] = (),
    font: ReSwingValue[Font] = (),
    enabled: ReSwingValue[Boolean] = (),
    minimumSize: ReSwingValue[Dimension] = (),
    maximumSize: ReSwingValue[Dimension] = (),
    preferredSize: ReSwingValue[Dimension] = ())
  extends
    ReComponent(background, foreground, font, enabled,
                minimumSize, maximumSize, preferredSize) {
  override protected lazy val peer = new ListView[A] with ComponentMixin
  
  protected val javaPeer = peer.peer.asInstanceOf[javax.swing.JList[_]]
  
  listData using (peer.listData _, peer.listData_= _, classOf[ListChanged[_]])
  visibleRowCount using (peer.visibleRowCount _, peer.visibleRowCount_= _, "visibleRowCount")
  selectionForeground using (peer.selectionForeground _, peer.selectionForeground_= _, "selectionForeground")
  selectionBackground using (peer.selectionBackground _, peer.selectionBackground_= _, "selectionBackground")
  
  selectIndices using peer.selectIndices _
  ensureIndexIsVisible using peer.ensureIndexIsVisible _
  
  val contentsChanged = ReSwingEvent using classOf[ListChanged[A]]
  val intervalRemoved = ReSwingEvent using classOf[ListElementsRemoved[A]]
  val intervalAdded = ReSwingEvent using classOf[ListElementsAdded[A]]
  
  class ReSelection(
      intervalMode: ReSwingValue[IntervalMode.Value]) {
    protected[ReListView] val peer = ReListView.this.peer.selection
    
    val leadIndex = ReSwingValue using (peer.leadIndex _, (peer, classOf[ListSelectionChanged[_]]))
    val anchorIndex = ReSwingValue using (peer.anchorIndex _, (peer, classOf[ListSelectionChanged[_]]))
    val indices = ReSwingValue using (
        { () => javaPeer.getSelectedIndices.toSet },
        (peer, classOf[ListSelectionChanged[_]]))
    val items = ReSwingValue using (
        { () => javaPeer.getSelectedValuesList.asScala.map(_.asInstanceOf[A]).toSeq },
        (peer, classOf[ListSelectionChanged[_]]))
    
    intervalMode using (peer.intervalMode _, peer.intervalMode_= _)
    
    val changed = ReSwingEvent using (peer, classOf[ListSelectionChanged[A]])
  }
  
  object ReSelection {
    implicit def toSelection(selection: ReSelection) = selection.peer
  }
  
  object selection extends ReSelection(`selection.intervalMode`)
}

object ReListView {
  implicit def toListView[A](component: ReListView[A]): ListView[A] = component.peer
}
