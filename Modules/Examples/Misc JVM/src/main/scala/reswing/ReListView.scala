package reswing

import scala.annotation.nowarn
import scala.jdk.CollectionConverters.*
import scala.swing.ListView.IntervalMode
import scala.swing.event.{ListChanged, ListElementsAdded, ListElementsRemoved, ListSelectionChanged}
import scala.swing.{Color, Dimension, Font, ListView}

@nowarn("msg=shadows field")
class ReListView[A](
    val listData: ReSwingValue[Seq[A]] = ReSwingNoValue[Seq[A]](),
    val visibleRowCount: ReSwingValue[Int] = (),
    val selectionForeground: ReSwingValue[Color] = (),
    val selectionBackground: ReSwingValue[Color] = (),
    selectIndices: ReSwingEvent[Seq[Int]] = (),
    ensureIndexIsVisible: ReSwingEvent[Int] = (),
    `selection.listDataSync`: ReSwingValue[Boolean] = (),
    `selection.intervalMode`: ReSwingValue[IntervalMode.Value] = (),
    background: ReSwingValue[Color] = (),
    foreground: ReSwingValue[Color] = (),
    font: ReSwingValue[Font] = (),
    enabled: ReSwingValue[Boolean] = (),
    minimumSize: ReSwingValue[Dimension] = (),
    maximumSize: ReSwingValue[Dimension] = (),
    preferredSize: ReSwingValue[Dimension] = ()
) extends ReComponent(background, foreground, font, enabled, minimumSize, maximumSize, preferredSize) {
  final override protected lazy val peer: ListView[A] & ComponentMixin = new ListView[A] with ComponentMixin

  protected val javaPeer = peer.peer.asInstanceOf[javax.swing.JList[A]]

  private var model: javax.swing.ListModel[A] = scala.compiletime.uninitialized

  private val modelListener = new javax.swing.event.ListDataListener {
    def contentsChanged(e: javax.swing.event.ListDataEvent): Unit = { peer publish ListChanged(peer) }
    def intervalRemoved(e: javax.swing.event.ListDataEvent): Unit = {
      peer publish ListElementsRemoved(peer, e.getIndex0 to e.getIndex1)
    }
    def intervalAdded(e: javax.swing.event.ListDataEvent): Unit = {
      peer publish ListElementsAdded(peer, e.getIndex0 to e.getIndex1)
    }
  }

  def modelChanged(): Unit = {
    if model != null then
      model `removeListDataListener` modelListener
    if javaPeer.getModel != null then
      javaPeer.getModel `addListDataListener` modelListener
    model = javaPeer.getModel
  }

  javaPeer `setModel` new ReListView.ReListModel[A]
  modelChanged()

  listData.using(
    { () => peer.listData.toSeq },
    { listData =>
      val selected =
        if selection.listDataSync.get then javaPeer.getSelectedValuesList.asScala.toSet
        else null

      (javaPeer.getModel match {
        case model: ReListView.ReListModel[A] => model
        case _ =>
          val model = new ReListView.ReListModel[A]
          javaPeer `setModel` model
          modelChanged()
          model
      })() = listData

      if selected != null then
        javaPeer `setSelectedIndices` (listData.zipWithIndex collect {
          case (el, index) if selected contains el => index
        }).toArray
    },
    classOf[ListChanged[?]]
  )

  visibleRowCount.using({ () => peer.visibleRowCount }, peer.visibleRowCount_=, "visibleRowCount")
  selectionForeground.using({ () => peer.selectionForeground }, peer.selectionForeground_=, "selectionForeground")
  selectionBackground.using({ () => peer.selectionBackground }, peer.selectionBackground_=, "selectionBackground")

  selectIndices using { () => peer.selectIndices() }
  ensureIndexIsVisible using { peer.ensureIndexIsVisible }

  val contentsChanged = ReSwingEvent `using` classOf[ListChanged[A]]
  val intervalRemoved = ReSwingEvent `using` classOf[ListElementsRemoved[A]]
  val intervalAdded   = ReSwingEvent `using` classOf[ListElementsAdded[A]]

  class ReSelection(
      val intervalMode: ReSwingValue[IntervalMode.Value],
      val listDataSync: ReSwingValue[Boolean]
  ) {
    protected[ReListView] val peer: ReListView.this.peer.selection.type = ReListView.this.peer.selection

    private[ReListView] var listDataSyncVar = false

    val leadIndex   = ReSwingValue.using({ () => peer.leadIndex }, (peer, classOf[ListSelectionChanged[?]]))
    val anchorIndex = ReSwingValue.using({ () => peer.anchorIndex }, (peer, classOf[ListSelectionChanged[?]]))
    val indices = ReSwingValue.using(
      { () => javaPeer.getSelectedIndices.toSet },
      (peer, classOf[ListSelectionChanged[?]])
    )
    val items = ReSwingValue.using(
      { () => javaPeer.getSelectedValuesList.asScala.toSeq },
      (peer, classOf[ListSelectionChanged[?]]),
      classOf[ListChanged[?]]
    )

    val index = ReSwingValue.using(
      { () => javaPeer.getSelectedIndex },
      (peer, classOf[ListSelectionChanged[?]])
    )
    val item = ReSwingValue.using(
      { () => Option(javaPeer.getSelectedValue) },
      (peer, classOf[ListSelectionChanged[?]]),
      classOf[ListChanged[?]]
    )

    intervalMode.using({ () => peer.intervalMode }, peer.intervalMode_=)
    listDataSync.using({ () => listDataSyncVar }, listDataSyncVar = _)

    val changed = ReSwingEvent.using(peer, classOf[ListSelectionChanged[A]])
  }

  object ReSelection {
    implicit def toSelection(selection: ReSelection): selection.peer.type = selection.peer
  }

  object selection
      extends ReSelection(
        `selection.intervalMode`,
        `selection.listDataSync`
      )
}

object ReListView {
  implicit def toListView[A](component: ReListView[A]): ListView[A] = component.peer

  class ReListModel[A] extends javax.swing.AbstractListModel[A] {
    private var items: scala.collection.Seq[A] = Seq.empty[A]
    def update(listData: Seq[A]): Unit = {
      val itemsSize  = items.size
      val additional = listData.size - itemsSize
      items = listData

      if additional > 0 then
        fireIntervalAdded(this, itemsSize, listData.size - 1)
      if additional < 0 then
        fireIntervalRemoved(this, listData.size, itemsSize - 1)
      fireContentsChanged(this, 0, listData.size)
    }
    def getElementAt(n: Int) = items(n)
    def getSize              = items.size
    def getItems             = items
  }
}
