package lofi_acl.example.monotonic_acl

import lofi_acl.access.Permission.{ALLOW, PARTIAL}
import lofi_acl.access.{Filter, Permission, PermissionTree}
import lofi_acl.example.monotonic_acl.PermissionTreePane.{ExpensePermCheckBoxes, ExpensePermEntryCheckBoxes, wiredReadWriteCheckboxes}
import lofi_acl.example.travelplanner.TravelPlan
import scalafx.beans.property.BooleanProperty
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control.{CheckBox, ContentDisplay, Label}
import scalafx.scene.layout.GridPane
import scalafx.scene.text.Text

class PermissionTreePane(
    rdt: TravelPlan,
    localReadPerm: PermissionTree,
    localWritePerm: PermissionTree
) extends GridPane {

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
  private val expensesParentCheckBoxes =
    ExpensePermCheckBoxes(globalReadCheckBox.selected, globalWriteCheckBox.selected)

  private val bucketListEntryCheckBoxes = rdt.bucketList.data.inner.map { (id, description) =>
    val (read, write) = wiredReadWriteCheckboxes(bucketListReadCheckBox.selected, bucketListWriteCheckBox.selected)
    id -> (description.value.read, read, write)
  }

  private val expenseEntryCheckBoxes = rdt.expenses.data.inner.map { (id, expense) =>
    id -> ExpensePermEntryCheckBoxes(
      expense.value.description.read.getOrElse("N/A"),
      parentRead = expensesParentCheckBoxes.read.selected,
      parentWrite = expensesParentCheckBoxes.write.selected,
      expensesParentCheckBoxes.descriptionRead.selected,
      expensesParentCheckBoxes.descriptionWrite.selected,
      expensesParentCheckBoxes.amountRead.selected,
      expensesParentCheckBoxes.amountWrite.selected,
      expensesParentCheckBoxes.commentRead.selected,
      expensesParentCheckBoxes.commentWrite.selected,
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

  addParentRow("Expenses List", expensesParentCheckBoxes.read, expensesParentCheckBoxes.write, curRowIdx)
  add({ val t = Text("Name"); t.alignmentInParent = Pos.Center; t }, 4, curRowIdx, 2, 1)
  add({ val t = Text("Amount"); t.alignmentInParent = Pos.Center; t }, 6, curRowIdx, 2, 1)
  add({ val t = Text("Comment"); t.alignmentInParent = Pos.Center; t }, 8, curRowIdx, 2, 1)
  curRowIdx += 1

  addLabeled("R:", expensesParentCheckBoxes.descriptionRead, 4, curRowIdx)
  addLabeled("W:", expensesParentCheckBoxes.descriptionWrite, 5, curRowIdx)
  addLabeled("R:", expensesParentCheckBoxes.amountRead, 6, curRowIdx)
  addLabeled("W:", expensesParentCheckBoxes.amountWrite, 7, curRowIdx)
  addLabeled("R:", expensesParentCheckBoxes.commentRead, 8, curRowIdx)
  addLabeled("W:", expensesParentCheckBoxes.commentWrite, 9, curRowIdx)
  curRowIdx += 1

  expenseEntryCheckBoxes.foreach { (_, boxes) =>
    add(Text(" ↳ "), 0, curRowIdx)
    add(Text(boxes.description), 1, curRowIdx)
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

  disableCheckBoxesWithInsufficientPermissions(localReadPerm, localWritePerm)

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

  private def disableCheckBoxesWithInsufficientPermissions(
      readPerm: PermissionTree,
      writePerm: PermissionTree
  ): Unit = {
    if readPerm.permission != ALLOW then globalReadCheckBox.disable = true
    if writePerm.permission != ALLOW then globalWriteCheckBox.disable = true

    val title      = PermissionTree.fromPath("title")
    val bucketList = PermissionTree.fromPath("bucketList")
    val expenses   = PermissionTree.fromPath("expenses")

    if !(title <= writePerm) then titleWriteCheckBox.disable = true
    if !(title <= readPerm) then titleReadCheckBox.disable = true

    if !(bucketList <= readPerm) then bucketListReadCheckBox.disable = true
    if !(bucketList <= writePerm) then bucketListWriteCheckBox.disable = true
    if !(PermissionTree.fromPath("bucketList.data") <= writePerm)
    then
      // Children that are not readable won't be in the list, so no need to disable the permission check boxes for them
      val entryWritePerms = writePerm
        .children.getOrElse("bucketList", PermissionTree.empty)
        .children.getOrElse("data", PermissionTree.empty)
      bucketListEntryCheckBoxes.foreach { case (id, (_, _, writeBox)) =>
        if entryWritePerms.children.getOrElse(id, PermissionTree.empty).permission != ALLOW
        then writeBox.disable = true
      }

    if !(expenses <= readPerm)
    then
      expensesParentCheckBoxes.read.disable = true
      if !(PermissionTree.fromPath("expenses.data.*.value.description") <= readPerm)
      then expensesParentCheckBoxes.descriptionRead.disable = true
      if !(PermissionTree.fromPath("expenses.data.*.value.amount") <= readPerm)
      then expensesParentCheckBoxes.amountRead.disable = true
      if !(PermissionTree.fromPath("expenses.data.*.value.comment") <= readPerm)
      then expensesParentCheckBoxes.commentRead.disable = true
    if !(expenses <= writePerm)
    then
      expensesParentCheckBoxes.write.disable = true
      if !(PermissionTree.fromPath("expenses.data.*.value.description") <= writePerm)
      then expensesParentCheckBoxes.descriptionWrite.disable = true
      if !(PermissionTree.fromPath("expenses.data.*.value.amount") <= writePerm)
      then expensesParentCheckBoxes.amountWrite.disable = true
      if !(PermissionTree.fromPath("expenses.data.*.value.comment") <= writePerm)
      then expensesParentCheckBoxes.commentWrite.disable = true

    if !(PermissionTree.fromPath("expenses.data") <= writePerm)
    then
      val entryWritePerms = writePerm
        .children.getOrElse("expenses", PermissionTree.empty)
        .children.getOrElse("data", PermissionTree.empty)
      val entryReadPerms = readPerm
        .children.getOrElse("expenses", PermissionTree.empty)
        .children.getOrElse("data", PermissionTree.empty)
      expenseEntryCheckBoxes.foreach { case (id, checkBoxes) =>
        checkBoxes.disableCheckBoxesIfInsufficientPermissions(
          entryReadPerms.children.getOrElse(id, PermissionTree.empty),
          entryWritePerms.children.getOrElse(id, PermissionTree.empty)
        )
      }
  }

  def selectionToPermissions(using filter: Filter[TravelPlan]): (PermissionTree, PermissionTree) = {
    extension (permissionTree: PermissionTree)
      private def allowIfSelected(checkBox: CheckBox, paths: String*): PermissionTree =
        if checkBox.isSelected then permissionTree.merge(PermissionTree.fromPathSet(paths.toSet))
        else permissionTree

    var read  = if globalReadCheckBox.isSelected then PermissionTree.allow else PermissionTree.empty
    var write = if globalWriteCheckBox.isSelected then PermissionTree.allow else PermissionTree.empty

    read = read
      .allowIfSelected(titleReadCheckBox, "title")
      .allowIfSelected(bucketListReadCheckBox, "bucketList")

    write = write
      .allowIfSelected(titleWriteCheckBox, "title")
      .allowIfSelected(bucketListWriteCheckBox, "bucketList")

    val bucketListReadEntries = bucketListEntryCheckBoxes.flatMap { case (id, (_, entryRead, _)) =>
      if entryRead.isSelected then Some(id -> PermissionTree.allow) else None
    }
    if bucketListReadEntries.nonEmpty then
      read = read.merge(PermissionTree(
        PARTIAL,
        Map("bucketList" -> PermissionTree(
          PARTIAL,
          Map(
            "data"      -> PermissionTree(PARTIAL, bucketListReadEntries),
            "observed"  -> PermissionTree.allow,
            "deletions" -> PermissionTree.allow
          )
        ))
      ))

    // Bucket list entries
    bucketListEntryCheckBoxes.flatMap { case (id, (_, _, entryWrite)) =>
      if entryWrite.isSelected then Some(id -> PermissionTree.allow) else None
    } match
      case entries if entries.nonEmpty =>
        write = write.merge(PermissionTree(
          PARTIAL,
          Map("bucketList" -> PermissionTree(
            PARTIAL,
            Map(
              "data"     -> PermissionTree(PARTIAL, entries),
              "observed" -> PermissionTree.allow
            )
          ))
        ))
      case _ =>

    inline def expenseWildcardSubPermissionTree(field: String): IArray[String] = {
      IArray(s"expenses.data.*.value.$field", "expenses.data.*.dots", "expenses.observed")
    }

    // Expenses wildcard
    read = read
      .allowIfSelected(expensesParentCheckBoxes.read, "expenses")
      .allowIfSelected(expensesParentCheckBoxes.descriptionRead, expenseWildcardSubPermissionTree("description")*)
      .allowIfSelected(expensesParentCheckBoxes.amountRead, expenseWildcardSubPermissionTree("amount")*)
      .allowIfSelected(expensesParentCheckBoxes.commentRead, expenseWildcardSubPermissionTree("comment")*)

    write = write
      .allowIfSelected(expensesParentCheckBoxes.write, "expenses")
      .allowIfSelected(expensesParentCheckBoxes.descriptionWrite, expenseWildcardSubPermissionTree("description")*)
      .allowIfSelected(expensesParentCheckBoxes.amountWrite, expenseWildcardSubPermissionTree("amount")*)
      .allowIfSelected(expensesParentCheckBoxes.commentWrite, expenseWildcardSubPermissionTree("comment")*)

    // Specific expense entries
    val expenseEntryReadPerms = expenseEntryCheckBoxes.map { case (id, boxes) =>
      id -> PermissionTree.empty
        .allowIfSelected(boxes.descriptionRead, "value.description", "dots")
        .allowIfSelected(boxes.amountRead, "value.amount", "dots")
        .allowIfSelected(boxes.commentRead, "value.comment", "dots")
    }.filterNot(_._2.isEmpty)
    if expenseEntryReadPerms.nonEmpty then
      read = read.merge(PermissionTree(
        PARTIAL,
        Map("expenses" -> PermissionTree(
          PARTIAL,
          Map(
            "data"     -> PermissionTree(PARTIAL, expenseEntryReadPerms),
            "observed" -> PermissionTree.allow
          )
        ))
      ))

    val expenseEntryWritePerms = expenseEntryCheckBoxes.map { case (id, boxes) =>
      id -> PermissionTree.empty
        .allowIfSelected(boxes.descriptionWrite, "value.description", "dots")
        .allowIfSelected(boxes.amountWrite, "value.amount", "dots")
        .allowIfSelected(boxes.commentWrite, "value.comment", "dots")
    }.filterNot(_._2.isEmpty)
    if expenseEntryWritePerms.nonEmpty then
      write = write.merge(PermissionTree(
        PARTIAL,
        Map("expenses" -> PermissionTree(
          PARTIAL,
          Map(
            "data"     -> PermissionTree(PARTIAL, expenseEntryWritePerms),
            "observed" -> PermissionTree.allow
          )
        ))
      ))

    filter.validatePermissionTree(read)
    read = filter.minimizePermissionTree(read)
    filter.validatePermissionTree(write)
    write = filter.minimizePermissionTree(write)
    (read, write)
  }
}

object PermissionTreePane {
  // TODO: Add override for existing permission of user to grant permissions to
  private class ExpensePermEntryCheckBoxes(
      val description: String,
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

    def disableCheckBoxesIfInsufficientPermissions(
        entryReadPerm: PermissionTree,
        entryWritePerm: PermissionTree
    ): Unit = {
      if !(PermissionTree.fromPath("value.description") <= entryReadPerm) then descriptionRead.disable = true
      if !(PermissionTree.fromPath("value.amount") <= entryReadPerm) then amountRead.disable = true
      if !(PermissionTree.fromPath("value.comment") <= entryReadPerm) then commentRead.disable = true

      if !(PermissionTree.fromPath("value.description") <= entryWritePerm) then descriptionWrite.disable = true
      if !(PermissionTree.fromPath("value.amount") <= entryWritePerm) then amountWrite.disable = true
      if !(PermissionTree.fromPath("value.comment") <= entryWritePerm) then commentWrite.disable = true
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
