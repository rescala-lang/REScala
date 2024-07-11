package lofi_acl.example.monotonic_acl

import scalafx.application.Platform
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control.{Button, TextField}
import scalafx.scene.layout.{BorderPane, HBox, VBox}

class MainScene extends Scene {
  private val viewModel = new MainSceneViewModel()
  private val rootPane  = new BorderPane()

  { // Initial screen with creation / join functionality
    val createNewDocumentButton: Button = new Button("Create new Travel Plan Document")
    createNewDocumentButton.alignment = Pos.Center
    createNewDocumentButton.onAction = _ => viewModel.createNewDocumentButtonPressed()
    createNewDocumentButton.disable <== viewModel.documentIsOpen

    val invitationTextField = new TextField {
      promptText = "paste invitation here"
    }
    invitationTextField.disable <== viewModel.documentIsOpen
    invitationTextField.text <==> viewModel.inviteString

    val joinDocumentButton: Button = new Button("Join")
    joinDocumentButton.alignment = Pos.Center
    joinDocumentButton.onAction = _ => viewModel.joinDocumentButtonPressed()
    joinDocumentButton.disable <== viewModel.documentIsOpen || invitationTextField.text.isEmpty

    rootPane.center = VBox(
      createNewDocumentButton,
      HBox(
        invitationTextField,
        joinDocumentButton
      )
    )
  }

  { // Travel plan is shown
    val createInvitationButton = Button()
    createInvitationButton.text = "Share"
    createInvitationButton.onAction = ev => viewModel.createInviteButtonPressed()

    viewModel.documentIsOpen.onChange {
      Platform.runLater {
        rootPane.center = VBox(
          HBox(viewModel.titleTextField, createInvitationButton),
          viewModel.bucketListView,
          viewModel.expenseViewContainer,
        )
      }
    }
  }

  content = rootPane
}
