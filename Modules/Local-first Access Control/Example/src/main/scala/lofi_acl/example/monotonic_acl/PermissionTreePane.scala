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
  private val expensePermParentCheckBoxes =
    ExpensePermCheckBoxes(globalReadCheckBox.selected, globalWriteCheckBox.selected)

  private val bucketListEntryCheckBoxes = rdt.bucketList.data.inner.map { (id, description) =>
    val (read, write) = wiredReadWriteCheckboxes(bucketListReadCheckBox.selected, bucketListWriteCheckBox.selected)
    id -> (description.value.read, read, write)
  }

  private val expenseEntryCheckBoxes = rdt.expenses.data.inner.map { (id, expense) =>
    id -> ExpensePermEntryCheckBoxes(
      expense.value.description.read.getOrElse("N/A"),
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
      expensePermParentCheckBoxes.read.disable = true
      if !(PermissionTree.fromPath("expenses.data.*.description") <= readPerm)
      then expensePermParentCheckBoxes.descriptionRead.disable = true
      if !(PermissionTree.fromPath("expenses.data.*.amount") <= readPerm)
      then expensePermParentCheckBoxes.amountRead.disable = true
      if !(PermissionTree.fromPath("expenses.data.*.comment") <= readPerm)
      then expensePermParentCheckBoxes.commentRead.disable = true
    if !(expenses <= writePerm)
    then
      expensePermParentCheckBoxes.write.disable = true
      if !(PermissionTree.fromPath("expenses.data.*.description") <= writePerm)
      then expensePermParentCheckBoxes.descriptionWrite.disable = true
      if !(PermissionTree.fromPath("expenses.data.*.amount") <= writePerm)
      then expensePermParentCheckBoxes.amountWrite.disable = true
      if !(PermissionTree.fromPath("expenses.data.*.comment") <= writePerm)
      then expensePermParentCheckBoxes.commentWrite.disable = true

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
      private def allowIfSelected(checkBox: CheckBox, path: String): PermissionTree =
        if checkBox.isSelected then permissionTree.merge(PermissionTree.fromPath(path))
        else permissionTree

    // TODO: Also add missing (read?) permissions for Obrem, if adding permissions for data
    // TODO: Check Filter for ORMap.inner.key vs ORMap.key

    var read  = if globalReadCheckBox.isSelected then PermissionTree.allow else PermissionTree.empty
    var write = if globalWriteCheckBox.isSelected then PermissionTree.allow else PermissionTree.empty

    read = read.allowIfSelected(titleReadCheckBox, "title")
    write = write.allowIfSelected(titleWriteCheckBox, "title")

    read = read.allowIfSelected(bucketListReadCheckBox, "bucketList")
    write = write.allowIfSelected(bucketListWriteCheckBox, "bucketList")

    val bucketListReadEntries = bucketListEntryCheckBoxes.flatMap { case (id, (_, entryRead, _)) =>
      if entryRead.isSelected then Some(id -> PermissionTree.allow) else None
    }
    read = read.merge(PermissionTree(
      PARTIAL,
      Map("bucketList" -> PermissionTree(PARTIAL, Map("data" -> PermissionTree(PARTIAL, bucketListReadEntries))))
    ))

    val bucketListWriteEntries = bucketListEntryCheckBoxes.flatMap { case (id, (_, _, entryWrite)) =>
      if entryWrite.isSelected then Some(id -> PermissionTree.allow) else None
    }
    write = write.merge(PermissionTree(
      PARTIAL,
      Map("bucketList" -> PermissionTree(PARTIAL, Map("data" -> PermissionTree(PARTIAL, bucketListWriteEntries))))
    ))

    read = read.allowIfSelected(expensePermParentCheckBoxes.read, "expenses")
    write = write.allowIfSelected(expensePermParentCheckBoxes.write, "expenses")

    read = read.allowIfSelected(expensePermParentCheckBoxes.descriptionRead, "expenses.data.*.description")
    write = write.allowIfSelected(expensePermParentCheckBoxes.descriptionWrite, "expenses.data.*.description")
    read = read.allowIfSelected(expensePermParentCheckBoxes.amountRead, "expenses.data.*.amount")
    write = write.allowIfSelected(expensePermParentCheckBoxes.amountWrite, "expenses.data.*.amount")
    read = read.allowIfSelected(expensePermParentCheckBoxes.commentRead, "expenses.data.*.comment")
    write = write.allowIfSelected(expensePermParentCheckBoxes.commentWrite, "expenses.data.*.comment")

    val expenseEntryPerms = expenseEntryCheckBoxes.map { case (id, boxes) =>
      var read  = PermissionTree.empty
      var write = PermissionTree.empty

      read = read.allowIfSelected(boxes.descriptionRead, "description")
      write = write.allowIfSelected(boxes.descriptionWrite, "description")
      read = read.allowIfSelected(boxes.amountRead, "amount")
      write = write.allowIfSelected(boxes.amountWrite, "amount")
      read = read.allowIfSelected(boxes.commentRead, "comment")
      write = write.allowIfSelected(boxes.commentWrite, "comment")

      id -> (read, write)
    }

    read = read.merge(PermissionTree(
      PARTIAL,
      Map("expenses" -> PermissionTree(
        PARTIAL,
        Map("data" -> PermissionTree(PARTIAL, expenseEntryPerms.map { case (id, (read, _)) => id -> read }))
      ))
    ))
    write = write.merge(PermissionTree(
      PARTIAL,
      Map("expenses" -> PermissionTree(
        PARTIAL,
        Map("data" -> PermissionTree(PARTIAL, expenseEntryPerms.map { case (id, (_, write)) => id -> write }))
      ))
    ))

    read = filter.minimizePermissionTree(read)
    filter.validatePermissionTree(read)
    write = filter.minimizePermissionTree(write)
    filter.validatePermissionTree(write)
    (read, write)
  }
}

object PermissionTreePane {
  // TODO: Add override from previous permission / max inheritable permission
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
      entryReadPerm match
        case PermissionTree(ALLOW, _) =>
        case PermissionTree(PARTIAL, children) =>
          read.disable = true
          children.get("description") match
            case Some(PermissionTree(ALLOW, _)) =>
            case _                              => descriptionRead.disable = true
          children.get("amount") match
            case Some(PermissionTree(ALLOW, _)) =>
            case _                              => amountRead.disable = true
          children.get("comment") match
            case Some(PermissionTree(ALLOW, _)) =>
            case _                              => commentRead.disable = true

      entryWritePerm match
        case PermissionTree(ALLOW, _) =>
        case PermissionTree(PARTIAL, children) =>
          write.disable = true
          children.get("description") match
            case Some(PermissionTree(ALLOW, _)) =>
            case _                              => descriptionWrite.disable = true
          children.get("amount") match
            case Some(PermissionTree(ALLOW, _)) =>
            case _                              => amountWrite.disable = true
          children.get("comment") match
            case Some(PermissionTree(ALLOW, _)) =>
            case _                              => commentWrite.disable = true
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
