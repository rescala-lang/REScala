package lofi_acl.example.monotonic_acl

import scalafx.application.Platform
import scalafx.beans.property.BooleanProperty
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control.{Button, TextField}
import scalafx.scene.layout.{BorderPane, HBox, VBox}

import scala.concurrent.ExecutionContext.global

// Initial screen with creation / join functionality
class MainScene extends Scene {
  private val rootPane = new BorderPane()

  val documentIsOpen: BooleanProperty = new BooleanProperty()
  documentIsOpen.value = false

  private val createNewDocumentButton: Button = new Button("Create new Travel Plan Document")
  createNewDocumentButton.alignment = Pos.Center
  createNewDocumentButton.onAction = _ => createNewDocumentButtonPressed()
  createNewDocumentButton.disable <== documentIsOpen

  private val invitationTextField = new TextField {
    promptText = "paste invitation here"
  }
  invitationTextField.disable <== documentIsOpen

  private val joinDocumentButton: Button = new Button("Join")
  joinDocumentButton.alignment = Pos.Center
  joinDocumentButton.onAction = _ => joinDocumentButtonPressed()
  joinDocumentButton.disable <== documentIsOpen || invitationTextField.text.isEmpty

  rootPane.center = VBox(
    createNewDocumentButton,
    HBox(
      invitationTextField,
      joinDocumentButton
    )
  )

  content = rootPane

  def createNewDocumentButtonPressed(): Unit = {
    documentIsOpen.value = true
    // TODO: Check if running this directly on the GUI Thread has better UX (glitches). Same on join.
    global.execute { () =>
      val travelPlanModel = TravelPlanModel.createNewDocument
      Platform.runLater {
        val travelPlanViewModel = TravelPlanViewModel(travelPlanModel)
        content = TravelPlanView(travelPlanViewModel)
        window.get().sizeToScene()
      }
    }
  }

  def joinDocumentButtonPressed(): Unit = {
    documentIsOpen.value = true
    global.execute { () =>
      val travelPlanModel = TravelPlanModel.joinDocument(invitationTextField.getText)
      Platform.runLater {
        val travelPlanViewModel = TravelPlanViewModel(travelPlanModel)
        content = TravelPlanView(travelPlanViewModel)
        window.get().sizeToScene()
      }
    }
  }
}
