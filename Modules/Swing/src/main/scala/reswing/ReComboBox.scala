package reswing

import scala.swing.event.{ListChanged, ListElementsAdded, ListElementsRemoved, SelectionChanged}
import scala.swing.{Color, ComboBox, Dimension, Font}

class ReComboBox[A](
    val items: ReSwingValue[Seq[A]] = ReSwingNoValue[Seq[A]](),
    `selection.index`: ReSwingValue[Int] = (),
    `selection.item`: ReSwingValue[Option[A]] = ReSwingNoValue[Option[A]](),
    background: ReSwingValue[Color] = (),
    foreground: ReSwingValue[Color] = (),
    font: ReSwingValue[Font] = (),
    enabled: ReSwingValue[Boolean] = (),
    minimumSize: ReSwingValue[Dimension] = (),
    maximumSize: ReSwingValue[Dimension] = (),
    preferredSize: ReSwingValue[Dimension] = ()
) extends ReComponent(background, foreground, font, enabled, minimumSize, maximumSize, preferredSize) {
  final override protected lazy val peer: ComboBox[A] & ComponentMixin =
    new ComboBox[A](Seq.empty[A]) with ComponentMixin

  protected val javaPeer = peer.peer.asInstanceOf[javax.swing.JComboBox[A]]

  private var model: javax.swing.ListModel[A] = scala.compiletime.uninitialized

  private val modelListener = new javax.swing.event.ListDataListener {
    def contentsChanged(e: javax.swing.event.ListDataEvent): Unit = { peer publish ListChanged(null) }
    def intervalRemoved(e: javax.swing.event.ListDataEvent): Unit = {
      peer publish ListElementsRemoved(null, e.getIndex0 to e.getIndex1)
    }
    def intervalAdded(e: javax.swing.event.ListDataEvent): Unit = {
      peer publish ListElementsAdded(null, e.getIndex0 to e.getIndex1)
    }
  }

  def modelChanged() = {
    if (model != null)
      model `removeListDataListener` modelListener
    if (javaPeer.getModel != null)
      javaPeer.getModel `addListDataListener` modelListener
    model = javaPeer.getModel
  }

  javaPeer `setModel` new ReComboBox.ReComboBoxModel[A]
  modelChanged()

  items.using(
    { () =>
      javaPeer.getModel match {
        case model: ReComboBox.ReComboBoxModel[A] => model.getItems
        case model                                => for (i <- 0 until model.getSize) yield model.getElementAt(i)
      }
    },
    { items =>
      val model = javaPeer.getModel match {
        case model: ReComboBox.ReComboBoxModel[A] => model
        case _ =>
          val model = new ReComboBox.ReComboBoxModel[A]
          javaPeer `setModel` model
          modelChanged()
          model
      }
      model.update(items)
    },
    classOf[ListChanged[?]]
  )

  class ReSelection(
      val index: ReSwingValue[Int],
      val item: ReSwingValue[Option[A]]
  ) {
    protected[ReComboBox] val peer: ReComboBox.this.peer.selection.type = ReComboBox.this.peer.selection

    index.using({ () => peer.index }, peer.index = _, (peer, classOf[SelectionChanged]))
    item.using(
      { () => Option(peer.item) },
      { item => peer.item = item getOrElse null.asInstanceOf[A] },
      (peer, classOf[SelectionChanged])
    )

    val changed = ReSwingEvent.using(peer, classOf[SelectionChanged])
  }

  object ReSelection {
    implicit def toSelection(selection: ReSelection): selection.peer.type = selection.peer
  }

  object selection extends ReSelection(`selection.index`, `selection.item`)
}

object ReComboBox {
  implicit def toComboBox[A](component: ReComboBox[A]): ComboBox[A] = component.peer

  class ReComboBoxModel[A] extends javax.swing.AbstractListModel[A] with javax.swing.ComboBoxModel[A] {
    private var items: Seq[A] = Seq.empty[A]

    def update(listData: Seq[A]): Unit = {
      val itemsSize: Int  = items.size
      val additional: Int = listData.size - itemsSize
      items = listData

      if (!items.contains[Any](selected)) selected = null

      if (additional > 0)
        fireIntervalAdded(this, itemsSize, listData.size - 1)
      if (additional < 0)
        fireIntervalRemoved(this, listData.size, itemsSize - 1)

      fireContentsChanged(this, 0, listData.size)
    }

    def getElementAt(n: Int) = items(n)
    def getSize              = items.size
    def getItems             = items

    private var selected: AnyRef = scala.compiletime.uninitialized
    def getSelectedItem: AnyRef  = selected
    def setSelectedItem(item: AnyRef): Unit = {
      if (
        (item == null || (items contains item)) &&
        ((selected != null && selected != item) ||
        (selected == null && item != null))
      ) {
        selected = item
        fireContentsChanged(this, -1, -1)
      }
    }
  }
}
