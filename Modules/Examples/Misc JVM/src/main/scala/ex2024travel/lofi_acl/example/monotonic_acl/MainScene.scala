package ex2024travel.lofi_acl.example.monotonic_acl

import javafx.stage.Stage
import scalafx.application.Platform
import scalafx.beans.property.BooleanProperty
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control.{Button, TextField}
import scalafx.scene.layout.{BorderPane, HBox, Priority, VBox}

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
      init(TravelPlanModel.createNewDocument)
    }
  }

  def joinDocumentButtonPressed(): Unit = {
    documentIsOpen.value = true
    val inviteString = invitationTextField.getText
    global.execute { () =>
      init(TravelPlanModel.joinDocument(inviteString))
    }
  }

  private def init(travelPlanModel: TravelPlanModel): Unit = {
    Platform.runLater {
      val travelPlanViewModel = TravelPlanViewModel(travelPlanModel)
      content = new HBox {
        children = TravelPlanView(travelPlanViewModel)
        hgrow = Priority.Always
      }
      val stage = window.get().asInstanceOf[Stage]
      stage.sizeToScene()
      stage.setTitle(s"TravelPlanner - replica ${travelPlanModel.publicId.id.take(10)}")
    }
  }
}
