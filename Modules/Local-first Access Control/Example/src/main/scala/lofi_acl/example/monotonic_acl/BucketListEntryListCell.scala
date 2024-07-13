package lofi_acl.example.monotonic_acl

import javafx.scene.control.ListCell
import scalafx.event.subscriptions.Subscription
import scalafx.scene.control.TextField

import scala.collection.mutable

class BucketListEntryListCell(travelPlanModel: TravelPlanModel) extends ListCell[String] {
  private val subscriptions = mutable.Buffer.empty[Subscription]

  // TODO: The old change listeners should be invalidated, when updateItem is called.
  override def updateItem(entryId: String, empty: Boolean): Unit = {
    super.updateItem(entryId, empty)

    subscriptions.foreach(_.cancel())
    subscriptions.clear()

    if empty || entryId == null then {
      setGraphic(null)
      return
    }

    val entryTextInModel = travelPlanModel.bucketListProperties.get()(entryId)
    val textField        = TextField()
    textField.text = entryTextInModel.get()
    // textField.hgrow = Priority.Always

    subscriptions += entryTextInModel.onChange((op, oldText, newText) =>
      if !textField.isFocused then
        textField.text = newText
    )
    subscriptions += textField.text.onChange((op, oldText, newText) =>
      if textField.isFocused then
        travelPlanModel.setBucketListEntryText(entryId, newText)
    )
    subscriptions += textField.focused.onChange((_, wasFocused, isNowFocused) =>
      if !isNowFocused then
        textField.text = entryTextInModel.get()
    )

    setGraphic(textField)
  }
}
