package lofi_acl.example.monotonic_acl

import javafx.scene.control.ListCell
import scalafx.scene.control.TextField

class BucketListEntryListCell(travelPlanModel: TravelPlanModel) extends ListCell[String] {
  // TODO: The old change listeners should be invalidated, when updateItem is called.
  override def updateItem(entryId: String, empty: Boolean): Unit = {
    super.updateItem(entryId, empty)

    if empty || entryId == null then {
      val oldGraphic = getGraphic
      setText(null)
      setGraphic(null)
      return
    }

    val entryTextInModel = travelPlanModel.bucketListProperties.get()(entryId)
    val textField        = TextField()
    textField.text = entryTextInModel.get()
    // textField.hgrow = Priority.Always

    entryTextInModel.onChange((op, oldText, newText) =>
      if !textField.isFocused then
        textField.text = newText
    )
    textField.text.onChange((op, oldText, newText) =>
      if textField.isFocused then
        travelPlanModel.setBucketListEntryText(entryId, newText)
    )
    textField.focused.onChange((_, wasFocused, isNowFocused) =>
      if !isNowFocused then
        textField.text = entryTextInModel.get()
    )

    setGraphic(textField)
  }
}
