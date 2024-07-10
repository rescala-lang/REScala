package lofi_acl.example.monotonic_acl

import lofi_acl.access.{Permission, PermissionTree}
import lofi_acl.example.monotonic_acl.PermissionTreePane.{ExpensePermCheckBoxes, ExpensePermEntryCheckBoxes, wiredReadWriteCheckboxes}
import lofi_acl.example.travelplanner.TravelPlan
import scalafx.beans.property.BooleanProperty
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.{CheckBox, ContentDisplay, Label}
import scalafx.scene.layout.GridPane
import scalafx.scene.text.Text

class PermissionTreePane(rdt: TravelPlan, localReadPerm: PermissionTree, localWritePerm: PermissionTree)
    extends GridPane {

  private var curRowIdx = 0
  vgap = 5
  hgap = 5
  padding = Insets(10)
  alignment = Pos.Center

  private val globalReadCheckBox  = CheckBox()
  private val globalWriteCheckBox = CheckBox()
  globalWriteCheckBox.selected.onChange((_, prev, cur) =>
    globalReadCheckBox.selected = cur
    globalReadCheckBox.disable = cur
  )

  private val (titleReadCheckBox, titleWriteCheckBox) =
    wiredReadWriteCheckboxes(globalReadCheckBox.selected, globalWriteCheckBox.selected)
  private val (bucketListReadCheckBox, bucketListWriteCheckBox) =
    wiredReadWriteCheckboxes(globalReadCheckBox.selected, globalWriteCheckBox.selected)
  private val expensePermParentCheckBoxes =
    ExpensePermCheckBoxes(globalReadCheckBox.selected, globalWriteCheckBox.selected)

  private val bucketListEntryCheckBoxes = rdt.bucketList.data.inner.map { (id, description) =>
    val (read, write) = wiredReadWriteCheckboxes(bucketListReadCheckBox.selected, bucketListWriteCheckBox.selected)
    id -> (description.read, read, write)
  }

  private val expenseEntryCheckBoxes = rdt.expenses.data.inner.map { (id, expense) =>
    id -> ExpensePermEntryCheckBoxes(
      expense.description.read.getOrElse("N/A"),
      parentRead = expensePermParentCheckBoxes.read.selected,
      parentWrite = expensePermParentCheckBoxes.write.selected,
      expensePermParentCheckBoxes.descriptionRead.selected,
      expensePermParentCheckBoxes.descriptionWrite.selected,
      expensePermParentCheckBoxes.amountRead.selected,
      expensePermParentCheckBoxes.amountWrite.selected,
      expensePermParentCheckBoxes.commentRead.selected,
      expensePermParentCheckBoxes.commentWrite.selected,
    )
  }

  addParentRow("Full Permission", globalReadCheckBox, globalWriteCheckBox, curRowIdx)
  curRowIdx += 1
  addParentRow("Title", titleReadCheckBox, titleWriteCheckBox, curRowIdx)
  curRowIdx += 1
  addParentRow("Bucket List", bucketListReadCheckBox, bucketListWriteCheckBox, curRowIdx)
  curRowIdx += 1

  bucketListEntryCheckBoxes.foreach { case (_, (description, readCheckBox, writeCheckBox)) =>
    add(Text(" ↳ "), 0, curRowIdx)
    add(Text(description), 1, curRowIdx)
    add(readCheckBox, 2, curRowIdx)
    add(writeCheckBox, 3, curRowIdx)
    readCheckBox.alignmentInParent = Pos.CenterRight
    writeCheckBox.alignmentInParent = Pos.CenterRight
    curRowIdx += 1
  }

  addParentRow("Expenses List", expensePermParentCheckBoxes.read, expensePermParentCheckBoxes.write, curRowIdx)
  add({ val t = Text("Name"); t.alignmentInParent = Pos.Center; t }, 4, curRowIdx, 2, 1)
  add({ val t = Text("Amount"); t.alignmentInParent = Pos.Center; t }, 6, curRowIdx, 2, 1)
  add({ val t = Text("Comment"); t.alignmentInParent = Pos.Center; t }, 8, curRowIdx, 2, 1)
  curRowIdx += 1

  addLabeled("R:", expensePermParentCheckBoxes.descriptionRead, 4, curRowIdx)
  addLabeled("W:", expensePermParentCheckBoxes.descriptionWrite, 5, curRowIdx)
  addLabeled("R:", expensePermParentCheckBoxes.amountRead, 6, curRowIdx)
  addLabeled("W:", expensePermParentCheckBoxes.amountWrite, 7, curRowIdx)
  addLabeled("R:", expensePermParentCheckBoxes.commentRead, 8, curRowIdx)
  addLabeled("W:", expensePermParentCheckBoxes.commentWrite, 9, curRowIdx)
  curRowIdx += 1

  // TODO: Add another header row for expenses
  expenseEntryCheckBoxes.foreach { (_, boxes) =>
    add(Text(" ↳ "), 0, curRowIdx)
    add(Text(boxes.title), 1, curRowIdx)
    add(boxes.read, 2, curRowIdx)
    add(boxes.write, 3, curRowIdx)
    add(boxes.descriptionRead, 4, curRowIdx)
    add(boxes.descriptionWrite, 5, curRowIdx)
    add(boxes.amountRead, 6, curRowIdx)
    add(boxes.amountWrite, 7, curRowIdx)
    add(boxes.commentRead, 8, curRowIdx)
    add(boxes.commentWrite, 9, curRowIdx)

    curRowIdx += 1
  }

  private def addParentRow(label: String, readCheckBox: CheckBox, writeCheckBox: CheckBox, rowIdx: Int): Unit = {
    add(Text(label), 0, rowIdx, 2, 1)
    addLabeled("READ:", readCheckBox, 2, rowIdx)
    addLabeled("WRITE:", writeCheckBox, 3, rowIdx)
  }

  private def addLabeled(
      labelString: String,
      checkBox: CheckBox,
      colIdx: Int,
      rowIdx: Int,
      colSpan: Int = 1,
      rowSpan: Int = 1
  ): Unit = {
    val lbl = Label(labelString, checkBox)
    lbl.contentDisplay = ContentDisplay.Right
    add(lbl, colIdx, rowIdx)
  }
}

object PermissionTreePane {
  // TODO: Add override from previous permission / max inheritable permission
  private class ExpensePermEntryCheckBoxes(
      val title: String,
      parentRead: BooleanProperty,
      parentWrite: BooleanProperty,
      parentDescriptionRead: BooleanProperty,
      parentDescriptionWrite: BooleanProperty,
      parentAmountRead: BooleanProperty,
      parentAmountWrite: BooleanProperty,
      parentCommentRead: BooleanProperty,
      parentCommentWrite: BooleanProperty,
  ) {
    val (read, write) = wiredReadWriteCheckboxes(parentRead, parentWrite)
    val (descriptionRead, descriptionWrite) =
      wiredReadWriteCheckboxes(read.selected, write.selected, parentDescriptionRead, parentDescriptionWrite)
    val (amountRead, amountWrite) =
      wiredReadWriteCheckboxes(read.selected, write.selected, parentAmountRead, parentAmountWrite)
    val (commentRead, commentWrite) =
      wiredReadWriteCheckboxes(read.selected, write.selected, parentCommentRead, parentCommentWrite)

    Seq(read, write, descriptionRead, descriptionWrite, amountRead, amountWrite, commentRead, commentWrite).foreach {
      _.alignmentInParent = Pos.CenterRight
    }
  }

  private class ExpensePermCheckBoxes(parentRead: BooleanProperty, parentWrite: BooleanProperty) {
    val (read, write)                       = wiredReadWriteCheckboxes(parentRead, parentWrite)
    val (descriptionRead, descriptionWrite) = wiredReadWriteCheckboxes(read.selected, write.selected)
    val (amountRead, amountWrite)           = wiredReadWriteCheckboxes(read.selected, write.selected)
    val (commentRead, commentWrite)         = wiredReadWriteCheckboxes(read.selected, write.selected)
  }

  // TODO: Add override from previous permission / max inheritable permission
  private def wiredReadWriteCheckboxes(
      parentRead: BooleanProperty,
      parentWrite: BooleanProperty
  ): (CheckBox, CheckBox) = {
    val read  = CheckBox()
    val write = CheckBox()

    parentWrite.onChange((_, prev, cur) =>
      write.selected = cur
      write.disable = cur
    )

    write.selected.onChange((_, prev, cur) =>
      read.selected = cur || parentRead.value
      read.disable = cur || parentRead.value
    )

    parentRead.onChange((_, prev, cur) =>
      read.selected = cur || write.isSelected
      read.disable = cur || write.isSelected
    )

    (read, write)
  }

  private def wiredReadWriteCheckboxes(
      parent1Read: BooleanProperty,
      parent1Write: BooleanProperty,
      parent2Read: BooleanProperty,
      parent2Write: BooleanProperty
  ): (CheckBox, CheckBox) = {
    val read  = CheckBox()
    val write = CheckBox()

    parent1Write.onChange((_, prev, cur) =>
      write.selected = cur || parent2Write.value
      write.disable = cur || parent2Write.value
    )
    parent2Write.onChange((_, prev, cur) =>
      write.selected = cur || parent1Write.value
      write.disable = cur || parent1Write.value
    )

    write.selected.onChange((_, prev, cur) =>
      read.selected = cur || parent1Read.value || parent2Read.value
      read.disable = cur || parent1Read.value || parent2Read.value
    )

    parent1Read.onChange((_, prev, cur) =>
      read.selected = cur || write.isSelected || parent2Read.value
      read.disable = cur || write.isSelected || parent2Read.value
    )
    parent2Read.onChange((_, prev, cur) =>
      read.selected = cur || write.isSelected || parent1Read.value
      read.disable = cur || write.isSelected || parent1Read.value
    )

    (read, write)
  }
}
