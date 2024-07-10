package lofi_acl.example.monotonic_acl

import scalafx.application.Platform
import scalafx.beans.property.{BooleanProperty, StringProperty}
import scalafx.scene.control.TextField
import scalafx.scene.layout.StackPane
import scalafx.stage.Stage

import scala.concurrent.ExecutionContext.global

class MainSceneViewModel {
  @volatile private var model: TravelPlanModel = scala.compiletime.uninitialized

  val documentIsOpen: BooleanProperty = new BooleanProperty()
  documentIsOpen.value = false

  val inviteString: StringProperty = new StringProperty()

  // Travel Plan:
  val titleTextField: TextField = TextField()

  val bucketListViewContainer: StackPane = StackPane()
  bucketListViewContainer.minWidth = 400
  bucketListViewContainer.minHeight = 800

  val expenseViewContainer: StackPane = StackPane()
  expenseViewContainer.minWidth = 400
  expenseViewContainer.minHeight = 800

  def createInviteButtonPressed(): Unit = {
    global.execute { () =>
      val invitation = model.createInvitation
      Platform.runLater(() => {
        val scene = InvitationDialogScene(invitation, model)
        val stage = new Stage()
        stage.setTitle("New Invite")
        stage.scene = scene
        stage.show()
      })
    }
  }

  def createNewDocumentButtonPressed(): Unit = {
    documentIsOpen.value = true
    require(model == null)
    global.execute { () =>
      model = TravelPlanModel.createNewDocument
      init()
    }
  }

  def joinDocumentButtonPressed(): Unit = {
    documentIsOpen.value = true
    require(model == null)
    global.execute { () =>
      model = TravelPlanModel.joinDocument(inviteString.value)
      init()
    }
  }

  private def init(): Unit = {
    titleTextField.text <==> model.title
  }
}
