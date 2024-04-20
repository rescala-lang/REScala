package reswing

import scala.swing.{Color, Dimension, Font, Table}
import scala.swing.Table.{AutoResizeMode, ElementMode, IntervalMode}
import scala.swing.event.{TableChanged, TableColumnsSelected, TableRowsAdded, TableRowsRemoved, TableRowsSelected, TableStructureChanged, TableUpdated}

class ReTable[A <: AnyRef](
    val rowData: ReSwingValue[Seq[Seq[A]]] = ReSwingNoValue[Seq[Seq[A]]](),
    val columnNames: ReSwingValue[Seq[String]] = (),
    val editable: ReSwingValue[ReTable.Editable] = (),
    val rowHeight: ReSwingValue[Int] = (),
    val autoResizeMode: ReSwingValue[AutoResizeMode.Value] = (),
    val gridColor: ReSwingValue[Color] = (),
    val showHorizontalLines: ReSwingValue[Boolean] = (),
    val showVerticalLines: ReSwingValue[Boolean] = (),
    val fillsViewportHeight: ReSwingValue[Boolean] = (),
    val selectionForeground: ReSwingValue[Color] = (),
    val selectionBackground: ReSwingValue[Color] = (),
    selectColumnInterval: ReSwingEvent[(Int, Int)] = (),
    selectRowInterval: ReSwingEvent[(Int, Int)] = (),
    selectAll: ReSwingEvent[Unit] = (),
    clearSelection: ReSwingEvent[Unit] = (),
    `selection.intervalMode`: ReSwingValue[IntervalMode.Value] = (),
    `selection.elementMode`: ReSwingValue[ElementMode.Value] = (),
    background: ReSwingValue[Color] = (),
    foreground: ReSwingValue[Color] = (),
    font: ReSwingValue[Font] = (),
    enabled: ReSwingValue[Boolean] = (),
    minimumSize: ReSwingValue[Dimension] = (),
    maximumSize: ReSwingValue[Dimension] = (),
    preferredSize: ReSwingValue[Dimension] = ()
) extends ReComponent(background, foreground, font, enabled, minimumSize, maximumSize, preferredSize) {
  override protected lazy val peer: Table & ComponentMixin = new Table with ComponentMixin

  private var model: javax.swing.table.TableModel = scala.compiletime.uninitialized

  val modelListener = new javax.swing.event.TableModelListener {
    def tableChanged(e: javax.swing.event.TableModelEvent) =
      peer publish (
        e.getType match {
          case javax.swing.event.TableModelEvent.UPDATE =>
            if (
              e.getFirstRow == 0 &&
              e.getLastRow == Int.MaxValue &&
              e.getColumn == javax.swing.event.TableModelEvent.ALL_COLUMNS
            )
              TableChanged(peer)
            else if (e.getFirstRow == javax.swing.event.TableModelEvent.HEADER_ROW)
              TableStructureChanged(peer)
            else
              TableUpdated(peer, e.getFirstRow to e.getLastRow, e.getColumn)
          case javax.swing.event.TableModelEvent.INSERT =>
            TableRowsAdded(peer, e.getFirstRow to e.getLastRow)
          case javax.swing.event.TableModelEvent.DELETE =>
            TableRowsRemoved(peer, e.getFirstRow to e.getLastRow)
        }
      )
  }

  def modelChanged(): Unit = {
    if (model != null)
      model `removeTableModelListener` modelListener
    if (peer.peer.getModel != null)
      peer.peer.getModel `addTableModelListener` modelListener
    model = peer.peer.getModel
  }

  peer.peer `setModel` new ReTable.ReTableModel[A]
  modelChanged()

  rowData.using(
    { () =>
      peer.peer.getModel match {
        case model: ReTable.ReTableModel[A @unchecked] =>
          model.getRowData
        case model =>
          for (r <- 0 to model.getRowCount())
            yield {
              for (c <- 0 to model.getColumnCount())
                yield (model.getValueAt(r, c)).asInstanceOf[A]
            }
      }
    },
    { rowData =>
      (peer.peer.getModel match {
        case model: ReTable.ReTableModel[A @unchecked] => model
        case _ =>
          val model = new ReTable.ReTableModel[A]
          peer.peer `setModel` model
          modelChanged()
          model
      })() = Left(rowData)
    },
    classOf[TableChanged],
    classOf[TableRowsRemoved],
    classOf[TableRowsAdded],
    classOf[TableStructureChanged],
    classOf[TableUpdated]
  )

  columnNames.using(
    { () =>
      peer.peer.getModel match {
        case model: ReTable.ReTableModel[?] =>
          model.getColumnNames
        case model =>
          for (c <- 0 to model.getColumnCount())
            yield model `getColumnName` c
      }
    },
    { columnNames =>
      (peer.peer.getModel match {
        case model: ReTable.ReTableModel[?] => model
        case _ =>
          val model = new ReTable.ReTableModel[A]
          peer.peer `setModel` model
          modelChanged()
          model
      })() = Right(columnNames)
    },
    classOf[TableStructureChanged]
  )

  editable.using(
    { () =>
      peer.peer.getModel match {
        case model: ReTable.ReTableModel[?] =>
          model.getCellEditable
        case model =>
          (row, column) => model.isCellEditable(row, column)
      }
    },
    { editable =>
      (peer.peer.getModel match {
        case model: ReTable.ReTableModel[?] => model
        case _ =>
          val model = new ReTable.ReTableModel[A]
          peer.peer `setModel` model
          modelChanged()
          model
      }) `setCellEditable` editable
    },
    classOf[TableStructureChanged]
  )

  rowHeight.using({ () => peer.rowHeight }, peer.rowHeight = _, "rowHeight")
  autoResizeMode.using({ () => peer.autoResizeMode }, peer.autoResizeMode = _, "autoResizeMode")

  showHorizontalLines.using(
    () => peer.peer.getShowHorizontalLines(),
    { peer.peer.setShowHorizontalLines(_) },
    "showHorizontalLines"
  )
  showVerticalLines.using(
    () => peer.peer.getShowVerticalLines(),
    { peer.peer.setShowVerticalLines(_) },
    "showVerticalLines"
  )
  gridColor.using({ () => peer.gridColor }, peer.gridColor = _, "gridColor")
  fillsViewportHeight.using(
    () => peer.peer.getFillsViewportHeight(),
    { peer.peer.setFillsViewportHeight(_) },
    "fillsViewportHeight"
  )
  selectionForeground.using(() => peer.selectionForeground, peer.selectionForeground = _, "selectionForeground")
  selectionBackground.using({ () => peer.selectionBackground }, peer.selectionBackground = _, "selectionBackground")

  selectColumnInterval using { range =>
    if (range._1 == -1 || range._2 == -1)
      peer.peer.clearSelection
    else
      peer.peer.setColumnSelectionInterval(range._1, range._2)
  }
  selectRowInterval using { range =>
    if (range._1 == -1 || range._2 == -1)
      peer.peer.clearSelection
    else
      peer.peer.setRowSelectionInterval(range._1, range._2)
  }
  selectAll.using(() => peer.peer.selectAll())
  clearSelection.using(() => peer.peer.clearSelection())

  val changed          = ReSwingEvent `using` classOf[TableChanged]
  val structureChanged = ReSwingEvent `using` classOf[TableStructureChanged]
  val updated          = ReSwingEvent `using` classOf[TableUpdated]
  val rowsAdded        = ReSwingEvent `using` classOf[TableRowsAdded]
  val rowsRemoved      = ReSwingEvent `using` classOf[TableRowsRemoved]

  class ReSelection(
      val intervalMode: ReSwingValue[IntervalMode.Value],
      val elementMode: ReSwingValue[ElementMode.Value]
  ) {
    protected[ReTable] val peer = ReTable.this.peer.selection

    val columnLeadIndex = ReSwingValue.using(
      { () => peer.columns.leadIndex },
      (peer, classOf[TableColumnsSelected])
    )
    val columnAnchorIndex = ReSwingValue.using(
      { () => peer.columns.anchorIndex },
      (peer, classOf[TableColumnsSelected])
    )
    val rowLeadIndex = ReSwingValue.using(
      { () => peer.rows.leadIndex },
      (peer, classOf[TableRowsSelected])
    )
    val rowAnchorIndex = ReSwingValue.using(
      { () => peer.rows.anchorIndex },
      (peer, classOf[TableRowsSelected])
    )

    val columns = ReSwingValue.using(
      { () => peer.columns.toSet },
      (peer, classOf[TableColumnsSelected])
    )
    val rows = ReSwingValue.using(
      { () => peer.rows.toSet },
      (peer, classOf[TableRowsSelected])
    )
    val cells = ReSwingValue.using(
      { () => peer.cells.toSet },
      (peer, classOf[TableColumnsSelected]),
      (peer, classOf[TableRowsSelected])
    )

    intervalMode.using({ () => peer.intervalMode }, peer.intervalMode_=)
    elementMode.using(
      { () => peer.elementMode },
      peer.elementMode = _,
      "columnSelectionAllowed",
      "rowSelectionAllowed",
      "cellSelectionEnabled"
    )

    val columnsSelected = ReSwingEvent.using(peer, classOf[TableColumnsSelected])
    val rowsSelected    = ReSwingEvent.using(peer, classOf[TableRowsSelected])
  }

  object selection
      extends ReSelection(
        `selection.intervalMode`,
        `selection.elementMode`
      )
}

object ReTable {
  implicit def toTable[A <: AnyRef](component: ReTable[A]): Table = component.peer

  type Editable = (Int, Int) => Boolean
  object Editable {
    val All: Editable  = (_, _) => true
    val None: Editable = (_, _) => false
  }

  class ReTableModel[A <: AnyRef] extends javax.swing.table.AbstractTableModel {
    private var rowData            = Seq.empty[Seq[A]]
    private var columnNames        = Seq.empty[String]
    private var editable: Editable = scala.compiletime.uninitialized

    def update(values: Either[Seq[Seq[A]], Seq[String]]): Unit = {
      values match {
        case Left(data) =>
          rowData = data
          fireTableDataChanged
        case Right(names) =>
          columnNames = names
          fireTableStructureChanged
      }
    }

    def setCellEditable(cellEditable: Editable): Unit = {
      editable = cellEditable
    }

    def getRowData      = rowData
    def getColumnNames  = columnNames
    def getCellEditable = editable

    def getRowCount    = rowData.length
    def getColumnCount = columnNames.length
    def getValueAt(row: Int, col: Int) = {
      if (rowData.isDefinedAt(row)) {
        val data = rowData(row)
        if (data.isDefinedAt(col))
          data(col)
        else
          null
      } else
        null
    }

    override def getColumnName(column: Int) = columnNames(column).toString
    override def isCellEditable(row: Int, column: Int) =
      if (editable != null)
        editable(row, column)
      else
        false
  }
}
