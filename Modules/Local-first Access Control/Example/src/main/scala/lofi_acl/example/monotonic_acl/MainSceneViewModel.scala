package lofi_acl.example.monotonic_acl

import scalafx.beans.property.{BooleanProperty, StringProperty}
import scalafx.scene.layout.StackPane

import scala.concurrent.ExecutionContext.global

class MainSceneViewModel {
  private var model: Option[TravelPlanModel] = None

  val bucketListViewContainer: StackPane = new StackPane()
  bucketListViewContainer.minWidth = 400
  bucketListViewContainer.minHeight = 800

  val expenseViewContainer: StackPane = new StackPane()
  expenseViewContainer.minWidth = 400
  expenseViewContainer.minHeight = 800

  val documentIsOpen: BooleanProperty = new BooleanProperty()
  documentIsOpen.value = false

  val joinDocumentUri: StringProperty = new StringProperty()

  def createNewDocumentButtonPressed(): Unit = {
    require(model.isEmpty)
    documentIsOpen.value = true
    global.execute { () =>
      model = Some(TravelPlanModel(None))
    }
  }

  def joinDocumentButtonPressed(): Unit = {
    documentIsOpen.value = true
    global.execute { () =>
      model = Some(TravelPlanModel(Some(joinDocumentUri.value)))
    }
  }
}
