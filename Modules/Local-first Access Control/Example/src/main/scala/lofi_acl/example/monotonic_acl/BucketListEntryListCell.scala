package lofi_acl.example.monotonic_acl

import javafx.scene.control.ListCell
import scalafx.scene.control.TextField
import scalafx.scene.layout.Priority

class BucketListEntryListCell(travelPlanModel: TravelPlanModel) extends ListCell[String] {
  override def updateItem(id: String, empty: Boolean): Unit = {
    super.updateItem(id, empty)

    if empty || id == null then {
      setText(null)
      setGraphic(null)
      return
    }

    val textField = TextField()
    textField.hgrow = Priority.Always
    textField.text <==> travelPlanModel.bucketListProperties.get()(id)

    setGraphic(textField)
  }
}
