package lofi_acl.example.monotonic_acl

import scalafx.beans.property.{BooleanProperty, StringProperty}
import scalafx.scene.layout.StackPane

import scala.concurrent.ExecutionContext.global

class MainSceneViewModel {
  @volatile private var model: TravelPlanModel = scala.compiletime.uninitialized

  val bucketListViewContainer: StackPane = new StackPane()
  bucketListViewContainer.minWidth = 400
  bucketListViewContainer.minHeight = 800

  val expenseViewContainer: StackPane = new StackPane()
  expenseViewContainer.minWidth = 400
  expenseViewContainer.minHeight = 800

  val documentIsOpen: BooleanProperty = new BooleanProperty()
  documentIsOpen.value = false

  val inviteString: StringProperty = new StringProperty()

  def createNewDocumentButtonPressed(): Unit = {
    documentIsOpen.value = true
    require(model == null)
    global.execute { () =>
      model = TravelPlanModel.createNewDocument
    }
  }

  def joinDocumentButtonPressed(): Unit = {
    documentIsOpen.value = true
    require(model == null)
    global.execute { () =>
      model = TravelPlanModel.joinDocument(inviteString.value)
    }
  }
}
